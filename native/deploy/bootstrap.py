#!/usr/bin/env python3
"""Reconcile the bounded CAUSAL stream and operator-owned durable consumers."""

import argparse
import getpass
import json
import os
import secrets
import socket
import ssl
from pathlib import Path

from render_users import validate_identity


class ProtocolError(RuntimeError):
    pass


class NatsConnection:
    def __init__(self, host: str, port: int, tls_name: str, ca: Path,
                 user: str, password: str) -> None:
        context = ssl.create_default_context(cafile=str(ca))
        raw = socket.create_connection((host, port), timeout=5)
        self.socket = context.wrap_socket(raw, server_hostname=tls_name)
        self.socket.settimeout(8)
        self.buffer = self.socket.makefile("rb")
        greeting = self._line()
        if not greeting.startswith(b"INFO "):
            raise ProtocolError(f"expected INFO, received {greeting!r}")
        connect = {
            "lang": "causal-bootstrap",
            "version": "1",
            "verbose": False,
            "pedantic": True,
            "tls_required": True,
            "user": user,
            "pass": password,
        }
        self._send(b"CONNECT " + json.dumps(connect, separators=(",", ":")).encode()
                   + b"\r\nPING\r\n")
        self._wait_for_pong()
        self.sid = 1

    def close(self) -> None:
        try:
            self.buffer.close()
        finally:
            self.socket.close()

    def _send(self, data: bytes) -> None:
        self.socket.sendall(data)

    def _line(self) -> bytes:
        line = self.buffer.readline()
        if not line:
            raise ProtocolError("server closed the connection")
        if not line.endswith(b"\r\n"):
            raise ProtocolError("unterminated protocol line")
        return line[:-2]

    def _wait_for_pong(self) -> None:
        while True:
            line = self._line()
            if line == b"PONG":
                return
            if line == b"PING":
                self._send(b"PONG\r\n")
            elif line.startswith(b"-ERR"):
                raise ProtocolError(line.decode("utf-8", "replace"))

    def request(self, subject: str, payload: object) -> dict:
        encoded = (payload if isinstance(payload, bytes) else
                   json.dumps(payload, separators=(",", ":")).encode())
        inbox = f"_INBOX.CAUSAL.BOOTSTRAP.{secrets.token_hex(12)}"
        sid = self.sid
        self.sid += 1
        frame = (f"SUB {inbox} {sid}\r\nPUB {subject} {inbox} {len(encoded)}\r\n"
                 .encode() + encoded + b"\r\nPING\r\n")
        self._send(frame)
        response = None
        while response is None:
            line = self._line()
            if line == b"PING":
                self._send(b"PONG\r\n")
                continue
            if line.startswith(b"-ERR"):
                raise ProtocolError(line.decode("utf-8", "replace"))
            if line == b"PONG":
                continue
            if not line.startswith(b"MSG "):
                continue
            parts = line.decode("ascii").split()
            if len(parts) not in (4, 5):
                raise ProtocolError(f"malformed MSG: {line!r}")
            length = int(parts[-1])
            body = self.buffer.read(length)
            trailer = self.buffer.read(2)
            if len(body) != length or trailer != b"\r\n":
                raise ProtocolError("truncated MSG payload")
            if parts[1] == inbox:
                response = json.loads(body)
        self._send(f"UNSUB {sid}\r\n".encode())
        if not isinstance(response, dict):
            raise ProtocolError("JetStream response was not an object")
        return response


def checked(response: dict, operation: str) -> dict:
    error = response.get("error")
    if error:
        raise ProtocolError(f"{operation}: {error.get('description', error)}")
    return response


def missing(response: dict) -> bool:
    error = response.get("error")
    return isinstance(error, dict) and error.get("code") == 404


def desired_stream() -> dict:
    return {
        "name": "CAUSAL",
        "description": "Bounded room history for Causal Chat",
        "subjects": ["chat.room.*"],
        "retention": "limits",
        "max_consumers": 1024,
        "max_msgs": 50000,
        "max_msgs_per_subject": 500,
        "max_bytes": 536870912,
        "max_age": 2592000000000000,
        "max_msg_size": 65536,
        "storage": "file",
        "num_replicas": 1,
        "discard": "old",
        "duplicate_window": 120000000000,
        "deny_delete": True,
        "deny_purge": True,
    }


def desired_consumer(name: str) -> dict:
    return {
        "durable_name": name,
        "name": name,
        "description": f"Causal Chat replay cursor for {name}",
        "deliver_policy": "all",
        "ack_policy": "explicit",
        "ack_wait": 30000000000,
        "max_deliver": 10,
        "filter_subject": "chat.room.*",
        "replay_policy": "instant",
        "max_ack_pending": 500,
        "max_waiting": 64,
        "max_batch": 500,
        "num_replicas": 1,
    }


def load_members(path: Path) -> list[dict[str, str]]:
    roster = json.loads(path.read_text(encoding="utf-8"))
    raw = roster.get("members") if isinstance(roster, dict) else None
    if not isinstance(raw, list) or not raw:
        raise ValueError("members must be a non-empty array")
    return [validate_identity(value, f"members[{index}]")
            for index, value in enumerate(raw)]


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("roster", type=Path)
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=4222)
    parser.add_argument("--tls-name", required=True)
    parser.add_argument("--ca", type=Path, required=True)
    parser.add_argument("--user", default=os.getenv("CAUSAL_ADMIN_USER"))
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    if not args.user:
        parser.error("--user or CAUSAL_ADMIN_USER is required")
    password = os.getenv("CAUSAL_ADMIN_PASSWORD")
    if password is None:
        password = getpass.getpass("Causal NATS admin password: ")
    members = load_members(args.roster)
    client = NatsConnection(args.host, args.port, args.tls_name, args.ca,
                            args.user, password)
    try:
        info = client.request("$JS.API.STREAM.INFO.CAUSAL", b"")
        if args.check:
            checked(info, "inspect CAUSAL stream")
        elif missing(info):
            checked(client.request("$JS.API.STREAM.CREATE.CAUSAL", desired_stream()),
                    "create CAUSAL stream")
        else:
            checked(info, "inspect CAUSAL stream")
            checked(client.request("$JS.API.STREAM.UPDATE.CAUSAL", desired_stream()),
                    "update CAUSAL stream")

        consumer_states = []
        for member in members:
            name = member["consumer"]
            if not args.check:
                checked(client.request(
                    f"$JS.API.CONSUMER.CREATE.CAUSAL.{name}",
                    {"stream_name": "CAUSAL",
                     "config": desired_consumer(name), "action": ""}),
                    f"reconcile consumer {name}")
            response = checked(client.request(
                f"$JS.API.CONSUMER.INFO.CAUSAL.{name}", b""),
                f"inspect consumer {name}")
            consumer_states.append({
                "consumer": name,
                "pending": response.get("num_pending"),
                "ack_pending": response.get("num_ack_pending"),
            })
        stream = checked(client.request("$JS.API.STREAM.INFO.CAUSAL", b""),
                         "inspect reconciled stream")
        print(json.dumps({
            "stream": "CAUSAL",
            "messages": stream.get("state", {}).get("messages"),
            "bytes": stream.get("state", {}).get("bytes"),
            "consumers": consumer_states,
        }, indent=2, sort_keys=True))
    finally:
        client.close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
