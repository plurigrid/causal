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


def receive_frame(stream, output):
    while True:
        value = line(stream)
        if value == b"PING":
            output.sendall(b"PONG\r\n")
            continue
        if value == b"PONG":
            continue
        if value.startswith(b"-ERR"):
            raise ProbeError(value.decode("utf-8", "replace"))
        if not (value.startswith(b"MSG ") or value.startswith(b"HMSG ")):
            continue
        parts = value.decode("ascii").split()
        headers = value.startswith(b"HMSG ")
        valid_lengths = (5, 6) if headers else (4, 5)
        if len(parts) not in valid_lengths:
            raise ProbeError("malformed NATS message header")
        length = int(parts[-1])
        header_length = int(parts[-2]) if headers else 0
        block = stream.read(length)
        if len(block) != length or stream.read(2) != b"\r\n":
            raise ProbeError("truncated MSG payload")
        reply_index = -3 if headers else -2
        has_reply = len(parts) == max(valid_lengths)
        reply = parts[reply_index] if has_reply else ""
        if headers:
            header = block[:header_length].decode("utf-8", "replace")
            payload = block[header_length:]
            status_line = header.split("\r\n", 1)[0]
            if status_line.startswith("NATS/1.0 404") or status_line.startswith(
                    "NATS/1.0 408"):
                return None
        else:
            payload = block
        return parts[1], reply, payload


def receive_message(stream, output, subject, marker: str) -> str:
    while True:
        frame = receive_frame(stream, output)
        if frame is None:
            raise ProbeError("message request expired before marker arrived")
        received_subject, reply, payload = frame
        if subject is not None and received_subject != subject:
            continue
        decoded = json.loads(payload)
        if decoded.get("id") == marker:
            return reply


def receive_replay(stream, output, profile: dict, inbox: str,
                   marker: str) -> int:
    acknowledged = 0
    next_subject = ("$JS.API.CONSUMER.MSG.NEXT."
                    f"{profile['stream']}.{profile['consumer']}")
    request = b'{"batch":500,"expires":2000000000}'
    for _ in range(10):
        output.sendall(
            f"PUB {next_subject} {inbox} {len(request)}\r\n".encode()
            + request + b"\r\n")
        while True:
            frame = receive_frame(stream, output)
            if frame is None:
                break
            _subject, reply, payload = frame
            if not reply:
                raise ProbeError("durable message had no acknowledgement subject")
            # A malformed record must remain pending for inspection/redelivery.
            # Acknowledging first would turn a parse failure into silent loss.
            decoded = json.loads(payload)
            if not isinstance(decoded, dict):
                raise ProbeError("durable message payload was not a JSON object")
            output.sendall(f"PUB {reply} 0\r\n\r\n".encode())
            acknowledged += 1
            if decoded.get("id") == marker:
                output.sendall(b"PING\r\n")
                wait_for_pong(stream, output)
                return acknowledged
    raise ProbeError("marker did not arrive within 5000 replay messages")


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
    parser.add_argument("--jetstream", action="store_true",
                        help="pull and acknowledge through the profile consumer")
    args = parser.parse_args()

    if args.publish_only and args.jetstream:
        parser.error("--publish-only and --jetstream are mutually exclusive")

    profile = json.loads(args.profile.read_text(encoding="utf-8"))
    required = ["host", "port", "tls_name", "user", "password"]
    if args.jetstream:
        required.extend(("stream", "consumer"))
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
    acknowledged = 0

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
                inbox = args.subject
                if args.jetstream:
                    inbox = (f"_INBOX.CAUSAL.{profile['consumer']}."
                             + secrets.token_hex(12))
                if not args.publish_only:
                    tls.sendall(f"SUB {inbox} 1\r\nPING\r\n".encode())
                    wait_for_pong(stream, tls)
                frame = (f"PUB {args.subject} {len(payload)}\r\n".encode()
                         + payload + b"\r\nPING\r\n")
                tls.sendall(frame)
                if not args.publish_only:
                    if args.jetstream:
                        # The first PONG proves the publish reached the server
                        # before the durable pull request is issued.
                        wait_for_pong(stream, tls)
                        acknowledged = receive_replay(
                            stream, tls, profile, inbox, marker)
                    else:
                        receive_message(stream, tls, args.subject, marker)
                        wait_for_pong(stream, tls)
                else:
                    wait_for_pong(stream, tls)
            finally:
                stream.close()

    print(json.dumps({
        "action": ("publish" if args.publish_only else
                   "replay-roundtrip" if args.jetstream else "roundtrip"),
        "consumer": profile.get("consumer") if args.jetstream else None,
        "acknowledged": acknowledged if args.jetstream else None,
        "endpoint": f"{destination}:{port}",
        "marker": marker,
        "subject": args.subject,
        "tls_name": profile["tls_name"],
        "user": profile["user"],
    }, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
