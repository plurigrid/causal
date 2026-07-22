#!/usr/bin/env python3
"""Dependency-free TLS/authenticated NATS member readiness probe."""

import argparse
import json
import secrets
import socket
import ssl
from pathlib import Path


class ProbeError(RuntimeError):
    pass


def line(stream) -> bytes:
    value = stream.readline()
    if not value or not value.endswith(b"\r\n"):
        raise ProbeError("server closed the connection or sent a malformed line")
    return value[:-2]


def wait_for_pong(stream, output) -> None:
    while True:
        value = line(stream)
        if value == b"PONG":
            return
        if value == b"PING":
            output.sendall(b"PONG\r\n")
        elif value.startswith(b"-ERR"):
            raise ProbeError(value.decode("utf-8", "replace"))


def receive_message(stream, output, subject: str, marker: str) -> None:
    while True:
        value = line(stream)
        if value == b"PING":
            output.sendall(b"PONG\r\n")
            continue
        if value.startswith(b"-ERR"):
            raise ProbeError(value.decode("utf-8", "replace"))
        if not value.startswith(b"MSG "):
            continue
        parts = value.decode("ascii").split()
        if len(parts) not in (4, 5):
            raise ProbeError("malformed MSG header")
        length = int(parts[-1])
        payload = stream.read(length)
        if len(payload) != length or stream.read(2) != b"\r\n":
            raise ProbeError("truncated MSG payload")
        if parts[1] != subject:
            continue
        decoded = json.loads(payload)
        if decoded.get("id") != marker:
            continue
        return


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("profile", type=Path)
    parser.add_argument("--ca", required=True, type=Path)
    parser.add_argument("--connect-host",
                        help="TCP destination when profile DNS is unavailable")
    parser.add_argument("--connect-port", type=int,
                        help="TCP port when a local tunnel differs from profile")
    parser.add_argument("--subject", default="chat.room.lobby")
    parser.add_argument("--sender", default="member-probe")
    parser.add_argument("--text", default="independent member readiness probe")
    parser.add_argument("--publish-only", action="store_true")
    args = parser.parse_args()

    profile = json.loads(args.profile.read_text(encoding="utf-8"))
    required = ("host", "port", "tls_name", "user", "password")
    if any(not profile.get(key) for key in required):
        parser.error("profile is missing a required connection field")
    destination = args.connect_host or profile["host"]
    port = args.connect_port or int(profile["port"])
    context = ssl.create_default_context(cafile=str(args.ca))
    marker = "probe-" + secrets.token_hex(12)
    payload = json.dumps({
        "id": marker,
        "sender": args.sender,
        "text": args.text,
    }, separators=(",", ":")).encode()

    with socket.create_connection((destination, port),
                                  timeout=7) as raw:
        with context.wrap_socket(raw, server_hostname=profile["tls_name"]) as tls:
            tls.settimeout(8)
            stream = tls.makefile("rb")
            try:
                greeting = line(stream)
                if not greeting.startswith(b"INFO "):
                    raise ProbeError("server did not send a NATS INFO greeting")
                connect = json.dumps({
                    "lang": "causal-member-probe",
                    "version": "1",
                    "verbose": False,
                    "pedantic": True,
                    "tls_required": True,
                    "user": profile["user"],
                    "pass": profile["password"],
                }, separators=(",", ":")).encode()
                tls.sendall(b"CONNECT " + connect + b"\r\n")
                if not args.publish_only:
                    tls.sendall(f"SUB {args.subject} 1\r\nPING\r\n".encode())
                    wait_for_pong(stream, tls)
                frame = (f"PUB {args.subject} {len(payload)}\r\n".encode()
                         + payload + b"\r\nPING\r\n")
                tls.sendall(frame)
                if not args.publish_only:
                    receive_message(stream, tls, args.subject, marker)
                wait_for_pong(stream, tls)
            finally:
                stream.close()

    print(json.dumps({
        "action": "publish" if args.publish_only else "roundtrip",
        "endpoint": f"{destination}:{port}",
        "marker": marker,
        "subject": args.subject,
        "tls_name": profile["tls_name"],
        "user": profile["user"],
    }, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
