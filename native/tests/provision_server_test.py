#!/usr/bin/env python3
"""Focused invariants for the rootless service provisioner."""

import importlib.util
import json
import os
import sys
import tempfile
import unittest
from unittest import mock
from pathlib import Path


DEPLOY = Path(__file__).resolve().parents[1] / "deploy"
sys.path.insert(0, str(DEPLOY))
SPEC = importlib.util.spec_from_file_location(
    "provision_server", DEPLOY / "provision_server.py")
assert SPEC and SPEC.loader
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)

MEMBER_SPEC = importlib.util.spec_from_file_location(
    "causal_member_probe", DEPLOY / "member_probe.py")
assert MEMBER_SPEC and MEMBER_SPEC.loader
MEMBER = importlib.util.module_from_spec(MEMBER_SPEC)
MEMBER_SPEC.loader.exec_module(MEMBER)


class RecordingSocket:
    def __init__(self):
        self.frames = []

    def sendall(self, value):
        self.frames.append(value)


class ProvisionServerTest(unittest.TestCase):
    def test_pinned_linux_arm64_assets(self):
        server = MODULE.asset("nats-server", "linux", "arm64")
        cli = MODULE.asset("nats", "linux", "arm64")
        self.assertEqual(server[0], "nats-server-v2.14.3-linux-arm64.tar.gz")
        self.assertEqual(len(server[1]), 64)
        self.assertEqual(cli[0], "nats-0.4.0-linux-arm64.zip")
        self.assertEqual(len(cli[1]), 64)

    def test_host_validation_distinguishes_dns_and_ip(self):
        self.assertEqual(MODULE.checked_host("chat.example.org"),
                         ("chat.example.org", "DNS"))
        self.assertEqual(MODULE.checked_host("127.0.0.1"),
                         ("127.0.0.1", "IP"))
        for invalid in ("", " chat.example.org", "chat example", "host\nname"):
            with self.subTest(invalid=invalid):
                with self.assertRaises(MODULE.ProvisionError):
                    MODULE.checked_host(invalid)

    def test_members_reject_collision_and_reserved_admin(self):
        self.assertEqual(MODULE.checked_members(["amber", "violet"]),
                         ["amber", "violet"])
        for invalid in (["amber", "amber"], ["causal-operator"], ["bad.name"],
                        "amber", [7]):
            with self.subTest(invalid=invalid):
                with self.assertRaises(MODULE.ProvisionError):
                    MODULE.checked_members(invalid)

    def test_rendered_server_keeps_tls_and_bounded_store(self):
        state = Path("/private/causal")
        users = 'authorization { users: [] }\n'
        rendered = MODULE.render_server_config(state, "127.0.0.1", 44524, users)
        self.assertIn('listen: "127.0.0.1:44524"', rendered)
        self.assertIn("handshake_first: true", rendered)
        self.assertIn("max_file: 10GB", rendered)
        self.assertIn(str(state / "tls/server.key"), rendered)
        self.assertTrue(rendered.endswith("\n"))

    def test_ipv6_endpoints_are_bracketed(self):
        rendered = MODULE.render_server_config(
            Path("/private/causal"), "::1", 44524,
            'authorization { users: [] }\n')
        self.assertIn('listen: "[::1]:44524"', rendered)
        self.assertEqual(MODULE.endpoint("2001:db8::7", 4222),
                         "[2001:db8::7]:4222")
        self.assertEqual(MODULE.endpoint("[::]", 4222), "[::]:4222")

    def test_handoff_separates_route_from_tls_identity(self):
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            fake_cli = root / "nats"
            fake_cli.write_text(
                "#!/bin/sh\n"
                "printf '%s\\n' "
                "'$2a$11$5IyFt.zoF2G8OmzyNDEmb.p/9Fuj8grCPxpyc33wDKsTahKq1jNYq'\n",
                encoding="utf-8",
            )
            fake_cli.chmod(0o755)
            roster = MODULE.create_roster(
                root / "secrets", fake_cli, "100.107.33.61",
                "c124b1.pirate-dragon.ts.net", 44525, ["amber"])
            profile_path = root / "secrets/client-amber.json"
            profile = json.loads(profile_path.read_text(encoding="utf-8"))
            self.assertEqual(profile["host"], "100.107.33.61")
            self.assertEqual(profile["tls_name"],
                             "c124b1.pirate-dragon.ts.net")
            self.assertEqual(os.stat(profile_path).st_mode & 0o777, 0o600)
            self.assertNotIn(profile["password"], json.dumps(roster))

    def test_malformed_replay_is_not_acknowledged(self):
        profile = {"stream": "CAUSAL", "consumer": "amber"}
        for payload, error in ((b"not-json", json.JSONDecodeError),
                               (b"[]", MEMBER.ProbeError)):
            with self.subTest(payload=payload):
                output = RecordingSocket()
                with mock.patch.object(
                        MEMBER, "receive_frame",
                        return_value=("chat.room.lobby", "$JS.ACK.CAUSAL.bad",
                                      payload)):
                    with self.assertRaises(error):
                        MEMBER.receive_replay(
                            object(), output, profile, "_INBOX.test", "marker")
                self.assertTrue(output.frames[0].startswith(
                    b"PUB $JS.API.CONSUMER.MSG.NEXT.CAUSAL.amber "))
                self.assertFalse(any(b"$JS.ACK.CAUSAL.bad" in frame
                                     for frame in output.frames))

    def test_systemd_quote_handles_spaces_specifiers_and_controls(self):
        self.assertEqual(
            MODULE.systemd_quote('/tmp/a path/%i/"log"\n'),
            '"/tmp/a path/%%i/\\"log\\"\\n"')

    def test_resume_rejects_service_path_traversal_before_manifest_read(self):
        with tempfile.TemporaryDirectory() as raw:
            args = MODULE.parser().parse_args([
                raw, "--resume-service", "--service", "systemd-user",
                "--service-name", "../escape",
            ])
            with self.assertRaisesRegex(MODULE.ProvisionError,
                                        "invalid --service-name"):
                MODULE.resume_service(args)


if __name__ == "__main__":
    unittest.main()
