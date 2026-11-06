#!/usr/bin/env python3
"""Generate private per-member credentials and a bcrypt-only NATS roster.

Plaintext credentials are written only to mode-0600 handoff files. The server
roster contains bcrypt hashes and is suitable for render_users.py. Existing
credentials are never silently replaced.
"""

import argparse
import json
import os
import secrets
import subprocess
import tempfile
from pathlib import Path

from render_users import NAME, validate_identity


def atomic_private_json(path: Path, value: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    descriptor, temporary = tempfile.mkstemp(prefix=path.name + ".", dir=path.parent)
    try:
        os.fchmod(descriptor, 0o600)
        with os.fdopen(descriptor, "w", encoding="utf-8") as output:
            json.dump(value, output, indent=2, sort_keys=True)
            output.write("\n")
        os.replace(temporary, path)
    except BaseException:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass
        raise


def bcrypt(nats_cli: Path, password: str) -> str:
    environment = os.environ.copy()
    environment["PASSWORD"] = password
    result = subprocess.run(
        [str(nats_cli), "server", "passwd"],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        env=environment,
    )
    encoded = result.stdout.strip()
    validate_identity({"user": "probe", "bcrypt": encoded}, "generated")
    return encoded


def identity(nats_cli: Path, user: str, consumer: str) -> tuple[dict, str]:
    password = secrets.token_urlsafe(32)
    return {
        "user": user,
        "consumer": consumer,
        "bcrypt": bcrypt(nats_cli, password),
    }, password


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("output", type=Path)
    parser.add_argument("--nats-cli", required=True, type=Path)
    parser.add_argument("--host", required=True)
    parser.add_argument("--port", required=True, type=int)
    parser.add_argument("--member", action="append", required=True,
                        help="user[:consumer]; repeat for each independent cursor")
    args = parser.parse_args()

    if not args.nats_cli.is_file() or not os.access(args.nats_cli, os.X_OK):
        parser.error("--nats-cli must name an executable file")
    if not (1 <= args.port <= 65535):
        parser.error("--port must be between 1 and 65535")
    roster_path = args.output / "roster.json"
    if roster_path.exists():
        parser.error(f"refusing to replace existing credentials: {roster_path}")

    members = []
    seen_users = set()
    seen_consumers = set()
    for raw in args.member:
        user, separator, consumer = raw.partition(":")
        if not separator:
            consumer = user
        if not NAME.fullmatch(user) or not NAME.fullmatch(consumer):
            parser.error(f"invalid member identity: {raw}")
        if user in seen_users or consumer in seen_consumers:
            parser.error(f"duplicate member identity: {raw}")
        seen_users.add(user)
        seen_consumers.add(consumer)
        members.append((user, consumer))

    args.output.mkdir(parents=True, exist_ok=True, mode=0o700)
    os.chmod(args.output, 0o700)
    admin, admin_password = identity(args.nats_cli, "causal-operator",
                                     "causal-operator")
    roster = {"admin": admin, "members": []}
    atomic_private_json(args.output / "admin.json", {
        "user": admin["user"],
        "password": admin_password,
    })

    for user, consumer in members:
        record, password = identity(args.nats_cli, user, consumer)
        roster["members"].append(record)
        atomic_private_json(args.output / f"client-{user}.json", {
            "host": args.host,
            "port": args.port,
            "tls": True,
            "tls_name": args.host,
            "user": user,
            "password": password,
            "jetstream": True,
            "stream": "CAUSAL",
            "consumer": consumer,
        })

    atomic_private_json(roster_path, roster)
    print(json.dumps({
        "roster": str(roster_path),
        "members": [user for user, _ in members],
        "handoffs": [str(args.output / f"client-{user}.json")
                     for user, _ in members],
    }, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
