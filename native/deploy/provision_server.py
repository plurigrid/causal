#!/usr/bin/env python3
"""Provision a rootless, independently operable Causal NATS service.

The provisioner downloads checksum-pinned upstream binaries when explicit
binary paths are not supplied, creates a private CA and server certificate,
generates one bcrypt-backed identity and durable cursor per member, performs a
real TLS/auth/JetStream room roundtrip, and can install a user service.

Plaintext passwords are written only to mode-0600 JSON handoffs. They are never
printed, passed on a command line, or embedded in the service definition.
"""

from __future__ import annotations

import argparse
import hashlib
import ipaddress
import json
import os
import platform
import re
import secrets
import shutil
import socket
import ssl
import subprocess
import sys
import tarfile
import tempfile
import time
import urllib.request
import zipfile
from pathlib import Path

from generate_roster import atomic_private_json
from render_users import NAME, atomic_write, render


NATS_SERVER_VERSION = "2.14.3"
NATS_CLI_VERSION = "0.4.0"

ASSETS = {
    ("nats-server", "darwin", "arm64"): (
        "nats-server-v2.14.3-darwin-arm64.tar.gz",
        "e086395457a7a93a440433a446c9f161f917d35c0374640555123e8d8d4b7fe9",
    ),
    ("nats-server", "linux", "amd64"): (
        "nats-server-v2.14.3-linux-amd64.tar.gz",
        "f3d0c820c749f81d717310fb00d4903919e70e3e66b268bd352a088b9788eb93",
    ),
    ("nats-server", "linux", "arm64"): (
        "nats-server-v2.14.3-linux-arm64.tar.gz",
        "1759b6a0ddebade9471b7c02891dfaa8c73b526c6f3ce391d4e21ec3eceffab8",
    ),
    ("nats", "darwin", "arm64"): (
        "nats-0.4.0-darwin-arm64.zip",
        "39a68a0673f1b87d0887f48b02ca17f5d803f5689d4a9cff8e74b6411d6baef7",
    ),
    ("nats", "linux", "amd64"): (
        "nats-0.4.0-linux-amd64.zip",
        "8dbd437c826b953dbd7432cf890ef22ba3c33dccc3dce5e71b3e8d055427849c",
    ),
    ("nats", "linux", "arm64"): (
        "nats-0.4.0-linux-arm64.zip",
        "9ce0c8a6653cd697d0b32687fcb53b59c13a2ad7a6ade7af8ad8a1c0f7357a87",
    ),
}

DNS_NAME = re.compile(
    r"(?=^.{1,253}$)(?:[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}"
    r"[A-Za-z0-9])?\.)*[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}"
    r"[A-Za-z0-9])?$"
)
SERVICE_NAME = re.compile(r"^[A-Za-z0-9_.-]{1,80}$")


class ProvisionError(RuntimeError):
    pass


def normalized_platform() -> tuple[str, str]:
    system = platform.system().lower()
    if system not in ("darwin", "linux"):
        raise ProvisionError(f"unsupported operating system: {system}")
    machine = platform.machine().lower()
    architecture = {
        "aarch64": "arm64",
        "arm64": "arm64",
        "amd64": "amd64",
        "x86_64": "amd64",
    }.get(machine)
    if architecture is None:
        raise ProvisionError(f"unsupported architecture: {machine}")
    return system, architecture


def asset(kind: str, system: str, architecture: str) -> tuple[str, str, str]:
    try:
        filename, digest = ASSETS[(kind, system, architecture)]
    except KeyError as error:
        raise ProvisionError(
            f"no pinned {kind} asset for {system}/{architecture}"
        ) from error
    repository = "nats-server" if kind == "nats-server" else "natscli"
    version = NATS_SERVER_VERSION if kind == "nats-server" else NATS_CLI_VERSION
    tag = f"v{version}"
    url = f"https://github.com/nats-io/{repository}/releases/download/{tag}/{filename}"
    return filename, digest, url


def checked_host(value: object) -> tuple[str, str]:
    if (not isinstance(value, str) or not value or value != value.strip()
            or any(ord(char) < 33 for char in value)):
        raise ProvisionError("--host must be a DNS name or IP literal")
    try:
        parsed = ipaddress.ip_address(value)
    except ValueError:
        ascii_name = value.encode("idna").decode("ascii")
        if not DNS_NAME.fullmatch(ascii_name):
            raise ProvisionError("--host must be a valid DNS name or IP literal")
        return ascii_name.lower(), "DNS"
    return str(parsed), "IP"


def checked_members(raw_members: object) -> list[str]:
    if (not isinstance(raw_members, list) or not raw_members
            or not all(isinstance(value, str) for value in raw_members)):
        raise ProvisionError("at least one --member is required")
    members: list[str] = []
    for value in raw_members:
        if not NAME.fullmatch(value):
            raise ProvisionError(f"invalid member name: {value}")
        if value == "causal-operator" or value in members:
            raise ProvisionError(f"duplicate or reserved member name: {value}")
        members.append(value)
    return members


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for block in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def download(url: str, destination: Path, expected: str) -> None:
    request = urllib.request.Request(url, headers={"User-Agent": "causal-provisioner/1"})
    with urllib.request.urlopen(request, timeout=60) as response:
        with destination.open("wb") as output:
            shutil.copyfileobj(response, output)
    actual = sha256(destination)
    if actual != expected:
        destination.unlink(missing_ok=True)
        raise ProvisionError(
            f"checksum mismatch for {destination.name}: expected {expected}, got {actual}"
        )


def extract_binary(archive: Path, kind: str, destination: Path) -> None:
    expected_name = kind
    payload: bytes | None = None
    if archive.name.endswith(".zip"):
        with zipfile.ZipFile(archive) as source:
            candidates = [entry for entry in source.infolist()
                          if not entry.is_dir()
                          and Path(entry.filename).name == expected_name]
            if len(candidates) != 1:
                raise ProvisionError(f"archive did not contain one {expected_name} binary")
            payload = source.read(candidates[0])
    else:
        with tarfile.open(archive, mode="r:gz") as source:
            candidates = [entry for entry in source.getmembers()
                          if entry.isfile()
                          and Path(entry.name).name == expected_name]
            if len(candidates) != 1:
                raise ProvisionError(f"archive did not contain one {expected_name} binary")
            extracted = source.extractfile(candidates[0])
            if extracted is None:
                raise ProvisionError(f"could not extract {expected_name}")
            payload = extracted.read()
    if payload is None or not payload:
        raise ProvisionError(f"empty {expected_name} binary")
    destination.write_bytes(payload)
    destination.chmod(0o755)


def install_binary(kind: str, supplied: Path | None, output: Path,
                   system: str, architecture: str, downloads: Path) -> dict[str, str]:
    if supplied is not None:
        source = supplied.expanduser().resolve()
        if not source.is_file() or not os.access(source, os.X_OK):
            raise ProvisionError(f"{source} is not an executable file")
        shutil.copy2(source, output)
        output.chmod(0o755)
        provenance = "operator-supplied"
    else:
        filename, expected, url = asset(kind, system, architecture)
        archive = downloads / filename
        download(url, archive, expected)
        extract_binary(archive, kind, output)
        provenance = url
    command = [str(output), "-v"] if kind == "nats-server" else [str(output), "--version"]
    result = subprocess.run(command, check=True, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    required = NATS_SERVER_VERSION if kind == "nats-server" else NATS_CLI_VERSION
    if required not in result.stdout:
        raise ProvisionError(f"{kind} did not report required version {required}")
    return {
        "version": required,
        "sha256": sha256(output),
        "provenance": provenance,
    }


def run(command: list[str], *, environment: dict[str, str] | None = None,
        input_text: str | None = None) -> subprocess.CompletedProcess[str]:
    try:
        return subprocess.run(command, check=True, text=True, input=input_text,
                              env=environment, stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE)
    except subprocess.CalledProcessError as error:
        diagnostic = (error.stderr or error.stdout or "no diagnostic").strip()
        if len(diagnostic) > 2000:
            diagnostic = diagnostic[-2000:]
        program = Path(command[0]).name
        raise ProvisionError(
            f"{program} exited with status {error.returncode}: {diagnostic}"
        ) from error


def create_tls(tls: Path, host: str, host_kind: str, openssl: str) -> str:
    tls.mkdir(mode=0o700)
    ca_key = tls / "ca.key"
    ca_cert = tls / "ca.crt"
    server_key = tls / "server.key"
    request = tls / "server.csr"
    server_cert = tls / "server.crt"
    extensions = tls / "server.ext"

    # RSA-3072 is intentionally used instead of relying on the host OpenSSL's
    # EC parameter encoding. LibreSSL can emit explicit EC parameters that Go's
    # X.509 parser correctly refuses, while RSA PKCS#8 is portable across the
    # supported macOS, Linux, NATS and Haiku OpenSSL stacks.
    run([openssl, "genpkey", "-algorithm", "RSA", "-pkeyopt",
         "rsa_keygen_bits:3072", "-out", str(ca_key)])
    run([openssl, "req", "-x509", "-new", "-sha256", "-days", "3650",
         "-key", str(ca_key), "-subj", "/CN=Causal Chat Private CA",
         "-addext", "basicConstraints=critical,CA:TRUE,pathlen:0",
         "-addext", "keyUsage=critical,keyCertSign,cRLSign",
         "-out", str(ca_cert)])
    run([openssl, "genpkey", "-algorithm", "RSA", "-pkeyopt",
         "rsa_keygen_bits:3072", "-out", str(server_key)])
    run([openssl, "req", "-new", "-sha256", "-key", str(server_key),
         "-subj", f"/CN={host}", "-out", str(request)])
    extensions.write_text(
        "basicConstraints=critical,CA:FALSE\n"
        "keyUsage=critical,digitalSignature,keyEncipherment\n"
        "extendedKeyUsage=serverAuth\n"
        f"subjectAltName={host_kind}:{host}\n"
        "authorityKeyIdentifier=keyid,issuer\n"
        "subjectKeyIdentifier=hash\n",
        encoding="utf-8",
    )
    serial = "0x" + os.urandom(16).hex()
    run([openssl, "x509", "-req", "-sha256", "-days", "397",
         "-in", str(request), "-CA", str(ca_cert), "-CAkey", str(ca_key),
         "-set_serial", serial, "-extfile", str(extensions),
         "-out", str(server_cert)])
    request.unlink()
    extensions.unlink()
    ca_key.chmod(0o600)
    server_key.chmod(0o600)
    ca_cert.chmod(0o644)
    server_cert.chmod(0o644)
    # LibreSSL, still shipped by some supported macOS hosts, lacks OpenSSL's
    # -verify_hostname/-verify_ip flags. Verify the chain here; the mandatory
    # default smoke test then performs an actual hostname-checked TLS handshake
    # through Python's SSL stack before any credential is sent.
    run([openssl, "verify", "-CAfile", str(ca_cert), str(server_cert)])
    fingerprint = run([openssl, "x509", "-in", str(ca_cert), "-noout",
                       "-fingerprint", "-sha256"]).stdout.strip()
    return fingerprint.split("=", 1)[-1]


def bcrypt(nats_cli: Path, password: str) -> str:
    environment = os.environ.copy()
    environment["PASSWORD"] = password
    result = run([str(nats_cli), "server", "passwd"], environment=environment)
    encoded = result.stdout.strip()
    if not re.fullmatch(r"\$2[aby]\$\d\d\$[./A-Za-z0-9]{53}", encoded):
        raise ProvisionError("nats CLI returned an invalid bcrypt value")
    return encoded


def create_roster(secrets_dir: Path, nats_cli: Path, connect_host: str,
                  tls_name: str, port: int,
                  members: list[str]) -> dict[str, object]:
    secrets_dir.mkdir(mode=0o700)
    admin_password = secrets.token_urlsafe(32)
    admin = {
        "user": "causal-operator",
        "consumer": "causal-operator",
        "bcrypt": bcrypt(nats_cli, admin_password),
    }
    roster: dict[str, object] = {"admin": admin, "members": []}
    atomic_private_json(secrets_dir / "admin.json", {
        "user": admin["user"], "password": admin_password,
    })
    for member in members:
        password = secrets.token_urlsafe(32)
        record = {"user": member, "consumer": member,
                  "bcrypt": bcrypt(nats_cli, password)}
        roster["members"].append(record)  # type: ignore[union-attr]
        atomic_private_json(secrets_dir / f"client-{member}.json", {
            "host": connect_host,
            "port": port,
            "tls": True,
            "tls_name": tls_name,
            "user": member,
            "password": password,
            "jetstream": True,
            "stream": "CAUSAL",
            "consumer": member,
        })
    atomic_private_json(secrets_dir / "roster.json", roster)
    return roster


def render_server_config(final_state: Path, listen_host: str, listen_port: int,
                         users: str) -> str:
    quoted = lambda value: json.dumps(str(value), ensure_ascii=True)
    return "\n".join([
        'server_name: "causal"',
        f"listen: {quoted(endpoint(listen_host, listen_port))}",
        "",
        "max_payload: 1MB",
        "max_connections: 4096",
        "max_subscriptions: 256",
        'write_deadline: "10s"',
        "",
        "tls {",
        f"  cert_file: {quoted(final_state / 'tls/server.crt')}",
        f"  key_file: {quoted(final_state / 'tls/server.key')}",
        f"  ca_file: {quoted(final_state / 'tls/ca.crt')}",
        "  handshake_first: true",
        "  timeout: 2",
        '  min_version: "1.2"',
        "}",
        "",
        "jetstream {",
        f"  store_dir: {quoted(final_state / 'store')}",
        "  max_mem: 256MB",
        "  max_file: 10GB",
        "}",
        "",
        users.rstrip(),
        "",
    ])


def endpoint(host: str, port: int) -> str:
    bare = host.strip("[]")
    return f"[{bare}]:{port}" if ":" in bare else f"{bare}:{port}"


def local_connect_host(listen_host: str) -> str:
    if listen_host in ("0.0.0.0", "::", "[::]"):
        return "127.0.0.1" if listen_host == "0.0.0.0" else "::1"
    return listen_host.strip("[]")


def wait_for_listener(process: subprocess.Popen[bytes] | None, host: str,
                      port: int, ca: Path, tls_name: str) -> None:
    context = ssl.create_default_context(cafile=str(ca))
    deadline = time.monotonic() + 12
    while time.monotonic() < deadline:
        if process is not None and process.poll() is not None:
            raise ProvisionError(f"nats-server exited with status {process.returncode}")
        try:
            with socket.create_connection((host, port), timeout=0.5) as raw:
                with context.wrap_socket(raw, server_hostname=tls_name) as secured:
                    secured.settimeout(0.5)
                    greeting = b""
                    while len(greeting) < 5:
                        block = secured.recv(5 - len(greeting))
                        if not block:
                            break
                        greeting += block
                    if greeting == b"INFO ":
                        return
        except OSError:
            time.sleep(0.1)
    raise ProvisionError("nats-server did not become ready")


def bootstrap_and_probe(state: Path, host: str, port: int, tls_name: str,
                        member: str) -> dict[str, object]:
    admin = json.loads((state / "secrets/admin.json").read_text(encoding="utf-8"))
    environment = os.environ.copy()
    environment["CAUSAL_ADMIN_USER"] = admin["user"]
    environment["CAUSAL_ADMIN_PASSWORD"] = admin["password"]
    tools = state / "tools"
    bootstrap = run([
        sys.executable, str(tools / "bootstrap.py"),
        str(state / "secrets/roster.json"),
        "--host", host, "--port", str(port), "--tls-name", tls_name,
        "--ca", str(state / "tls/ca.crt"),
    ], environment=environment)
    state_result = json.loads(bootstrap.stdout)
    probe = run([
        sys.executable, str(tools / "member_probe.py"),
        str(state / f"secrets/client-{member}.json"),
        "--ca", str(state / "tls/ca.crt"),
        "--connect-host", host, "--connect-port", str(port),
        "--jetstream",
        "--sender", "independent-provisioner",
        "--text", "rootless provisioner end-to-end readiness",
    ])
    probe_result = json.loads(probe.stdout)
    postcheck = run([
        sys.executable, str(tools / "bootstrap.py"),
        str(state / "secrets/roster.json"),
        "--host", host, "--port", str(port), "--tls-name", tls_name,
        "--ca", str(state / "tls/ca.crt"), "--check",
    ], environment=environment)
    post_state = json.loads(postcheck.stdout)
    member_state = next((value for value in post_state.get("consumers", [])
                         if value.get("consumer") == member), None)
    if (member_state is None or member_state.get("pending") != 0
            or member_state.get("ack_pending") != 0):
        raise ProvisionError("durable replay cursor did not reach a fully acknowledged state")
    if post_state.get("messages", 0) < 1:
        raise ProvisionError("durable stream did not retain the readiness message")
    return {"bootstrap": state_result, "probe": {
        key: probe_result[key] for key in
        ("acknowledged", "action", "consumer", "endpoint", "subject",
         "tls_name", "user")
    }, "postcheck": post_state}


def smoke(state: Path, connect_host: str, listen_port: int, tls_name: str,
          member: str) -> dict[str, object]:
    log_path = state / "logs/smoke.log"
    log_path.touch(mode=0o600, exist_ok=True)
    log_path.chmod(0o600)
    with log_path.open("ab", buffering=0) as log:
        process = subprocess.Popen(
            [str(state / "bin/nats-server"), "-c", str(state / "config/server.conf")],
            stdin=subprocess.DEVNULL, stdout=log, stderr=subprocess.STDOUT,
            start_new_session=True,
        )
        try:
            wait_for_listener(process, connect_host, listen_port,
                              state / "tls/ca.crt", tls_name)
            result = bootstrap_and_probe(state, connect_host, listen_port,
                                         tls_name, member)
        finally:
            process.terminate()
            try:
                process.wait(timeout=8)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=3)
    if process.returncode not in (0, -15):
        raise ProvisionError(f"smoke server stopped with status {process.returncode}")
    return result


def systemd_quote(value: Path | str) -> str:
    escaped = (str(value)
               .replace("\\", "\\\\")
               .replace('"', '\\"')
               .replace("%", "%%")
               .replace("\n", "\\n")
               .replace("\r", "\\r")
               .replace("\t", "\\t"))
    return f'"{escaped}"'


def install_systemd_user(state: Path, service_name: str) -> Path:
    if platform.system() != "Linux":
        raise ProvisionError("systemd-user service is available only on Linux")
    unit_dir = Path.home() / ".config/systemd/user"
    unit_dir.mkdir(parents=True, exist_ok=True)
    unit = unit_dir / f"{service_name}.service"
    if unit.exists():
        raise ProvisionError(f"refusing to replace existing service: {unit}")
    content = "\n".join([
        "[Unit]",
        "Description=Causal Chat NATS service",
        "After=network-online.target",
        "Wants=network-online.target",
        "",
        "[Service]",
        "Type=simple",
        f"ExecStart={systemd_quote(state / 'bin/nats-server')} -c "
        f"{systemd_quote(state / 'config/server.conf')}",
        "Restart=on-failure",
        "RestartSec=2",
        "NoNewPrivileges=true",
        "PrivateTmp=true",
        "ProtectSystem=strict",
        "ProtectHome=read-only",
        f"ReadWritePaths={systemd_quote(state / 'store')} "
        f"{systemd_quote(state / 'logs')}",
        "ProtectKernelTunables=true",
        "ProtectControlGroups=true",
        "RestrictSUIDSGID=true",
        "LockPersonality=true",
        "RestrictAddressFamilies=AF_UNIX AF_INET AF_INET6",
        "StandardOutput=" + systemd_quote("append:" + str(state / "logs/server.log")),
        "StandardError=inherit",
        "",
        "[Install]",
        "WantedBy=default.target",
        "",
    ])
    atomic_write(unit, content)
    unit.chmod(0o600)
    try:
        run(["systemctl", "--user", "daemon-reload"])
        run(["systemctl", "--user", "enable", "--now", unit.name])
        active = run(["systemctl", "--user", "is-active", unit.name]).stdout.strip()
        if active != "active":
            raise ProvisionError(f"service did not become active: {active}")
    except BaseException:
        subprocess.run(["systemctl", "--user", "disable", "--now", unit.name],
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        unit.unlink(missing_ok=True)
        subprocess.run(["systemctl", "--user", "daemon-reload"],
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        raise
    return unit


def install_launchd(state: Path, service_name: str) -> Path:
    if platform.system() != "Darwin":
        raise ProvisionError("launchd service is available only on macOS")
    from plistlib import dump

    launch_dir = Path.home() / "Library/LaunchAgents"
    launch_dir.mkdir(parents=True, exist_ok=True)
    label = service_name if "." in service_name else f"info.nonlocal.{service_name}"
    plist = launch_dir / f"{label}.plist"
    if plist.exists():
        raise ProvisionError(f"refusing to replace existing service: {plist}")
    value = {
        "Label": label,
        "ProgramArguments": [str(state / "bin/nats-server"), "-c",
                             str(state / "config/server.conf")],
        "RunAtLoad": True,
        "KeepAlive": {"SuccessfulExit": False},
        "ProcessType": "Background",
        "StandardOutPath": str(state / "logs/server.log"),
        "StandardErrorPath": str(state / "logs/server.log"),
    }
    descriptor, temporary = tempfile.mkstemp(prefix=plist.name + ".",
                                             dir=launch_dir)
    try:
        os.fchmod(descriptor, 0o600)
        with os.fdopen(descriptor, "wb") as output:
            dump(value, output)
        os.replace(temporary, plist)
        domain = f"gui/{os.getuid()}"
        run(["launchctl", "bootstrap", domain, str(plist)])
        run(["launchctl", "kickstart", "-k", f"{domain}/{label}"])
    except BaseException:
        subprocess.run(["launchctl", "bootout", f"gui/{os.getuid()}", str(plist)],
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        Path(temporary).unlink(missing_ok=True)
        plist.unlink(missing_ok=True)
        raise
    return plist


def install_selected_service(state: Path, mode: str, service_name: str,
                             system: str) -> Path:
    selected = mode
    if selected == "auto":
        selected = "launchd" if system == "darwin" else "systemd-user"
    if selected == "systemd-user":
        return install_systemd_user(state, service_name)
    if selected == "launchd":
        return install_launchd(state, service_name)
    raise ProvisionError(f"unsupported service mode: {selected}")


def service_persistence(system: str, service_path: Path | None) -> str:
    if service_path is None:
        return "none"
    if system == "linux":
        result = subprocess.run(
            ["loginctl", "show-user", str(os.getuid()), "-p", "Linger", "--value"],
            text=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        if result.returncode == 0 and result.stdout.strip() == "yes":
            return "survives-logout"
    return "login-session"


def remove_service(path: Path) -> None:
    if path.suffix == ".service":
        subprocess.run(["systemctl", "--user", "disable", "--now", path.name],
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        path.unlink(missing_ok=True)
        subprocess.run(["systemctl", "--user", "daemon-reload"],
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    elif path.suffix == ".plist":
        label = path.stem
        subprocess.run(["launchctl", "bootout", f"gui/{os.getuid()}/{label}"],
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        path.unlink(missing_ok=True)


def resume_service(args: argparse.Namespace) -> dict[str, object]:
    state = args.state.expanduser().resolve()
    if not state.is_dir():
        raise ProvisionError(f"existing state directory not found: {state}")
    if args.service == "none":
        raise ProvisionError("--resume-service requires a service mode")
    if args.no_smoke:
        raise ProvisionError("--resume-service cannot skip its state check")
    if not SERVICE_NAME.fullmatch(args.service_name):
        raise ProvisionError("invalid --service-name")
    manifest = json.loads((state / "manifest.json").read_text(encoding="utf-8"))
    if not isinstance(manifest, dict) or manifest.get("schema") != 1:
        raise ProvisionError("unsupported or missing state manifest")
    host, _host_kind = checked_host(manifest.get("host", ""))
    profile_host, _connect_kind = checked_host(manifest.get("connect_host", host))
    members = checked_members(manifest.get("members", []))
    listen_host = manifest.get("listen_host")
    listen_port = manifest.get("listen_port")
    profile_port = manifest.get("port")
    if (not isinstance(listen_host, str)
            or not isinstance(listen_port, int) or isinstance(listen_port, bool)
            or not isinstance(profile_port, int) or isinstance(profile_port, bool)
            or not 1 <= listen_port <= 65535
            or not 1 <= profile_port <= 65535):
        raise ProvisionError("state manifest has an invalid listener")
    try:
        ipaddress.ip_address(listen_host.strip("[]"))
    except ValueError as error:
        raise ProvisionError("state manifest listener is not an IP literal") from error
    required = [state / "bin/nats-server", state / "config/server.conf",
                state / "tls/ca.crt", state / "secrets/roster.json",
                state / f"secrets/client-{members[0]}.json"]
    if any(not path.is_file() for path in required):
        raise ProvisionError("existing state is incomplete")
    system, _architecture = normalized_platform()
    service_path = install_selected_service(state, args.service,
                                            args.service_name, system)
    local_host = local_connect_host(listen_host)
    try:
        wait_for_listener(None, local_host, listen_port,
                          state / "tls/ca.crt", host)
        smoke_result = bootstrap_and_probe(state, local_host, listen_port,
                                           host, members[0])
    except BaseException:
        remove_service(service_path)
        raise
    return {
        "state": str(state),
        "endpoint": endpoint(profile_host, profile_port),
        "tls_name": host,
        "listen": endpoint(listen_host, listen_port),
        "members": members,
        "ca": str(state / "tls/ca.crt"),
        "ca_sha256_fingerprint": manifest["ca_sha256_fingerprint"],
        "handoffs": [str(state / f"secrets/client-{member}.json")
                     for member in members],
        "service": str(service_path),
        "service_persistence": service_persistence(system, service_path),
        "smoke": smoke_result,
        "resumed": True,
    }


def provision(args: argparse.Namespace) -> dict[str, object]:
    if args.resume_service:
        return resume_service(args)
    if args.host is None:
        raise ProvisionError("--host is required when creating state")
    if args.no_smoke and args.service != "none":
        raise ProvisionError("--no-smoke is only valid without a service")
    host, host_kind = checked_host(args.host)
    profile_host, _connect_kind = checked_host(args.connect_host or host)
    members = checked_members(args.member)
    if not (1 <= args.listen_port <= 65535 and 1 <= args.port <= 65535):
        raise ProvisionError("ports must be between 1 and 65535")
    try:
        ipaddress.ip_address(args.listen_host.strip("[]"))
    except ValueError as error:
        raise ProvisionError("--listen-host must be an IP literal") from error
    if not SERVICE_NAME.fullmatch(args.service_name):
        raise ProvisionError("invalid --service-name")
    system, architecture = normalized_platform()
    state = args.state.expanduser().resolve()
    if state.exists():
        raise ProvisionError(f"refusing to replace existing state: {state}")
    state.parent.mkdir(parents=True, exist_ok=True)
    staging = Path(tempfile.mkdtemp(prefix=state.name + ".", dir=state.parent))
    staging.chmod(0o700)
    try:
        for directory in ("bin", "config", "downloads", "logs", "store", "tools"):
            (staging / directory).mkdir(mode=0o700)
        atomic_write(staging / "logs/server.log", "")
        here = Path(__file__).resolve().parent
        tool_digests = {}
        for name in ("bootstrap.py", "generate_roster.py", "member_probe.py",
                     "provision_server.py", "render_users.py"):
            destination = staging / "tools" / name
            shutil.copy2(here / name, destination)
            destination.chmod(0o755)
            tool_digests[name] = sha256(destination)
        binaries = {
            "nats-server": install_binary(
                "nats-server", args.nats_server, staging / "bin/nats-server",
                system, architecture, staging / "downloads"),
            "nats": install_binary(
                "nats", args.nats_cli, staging / "bin/nats",
                system, architecture, staging / "downloads"),
        }
        fingerprint = create_tls(staging / "tls", host, host_kind, args.openssl)
        roster = create_roster(staging / "secrets", staging / "bin/nats",
                               profile_host, host, args.port, members)
        users = render(roster)
        atomic_write(staging / "config/users.conf", users)
        final_config = render_server_config(state, args.listen_host,
                                            args.listen_port, users)
        atomic_write(staging / "config/server.conf", final_config)
        manifest = {
            "schema": 1,
            "host": host,
            "connect_host": profile_host,
            "port": args.port,
            "listen_host": args.listen_host,
            "listen_port": args.listen_port,
            "members": members,
            "ca_sha256_fingerprint": fingerprint,
            "platform": {"system": system, "architecture": architecture},
            "binaries": binaries,
            "tools": tool_digests,
        }
        atomic_private_json(staging / "manifest.json", manifest)
        shutil.rmtree(staging / "downloads")
        os.replace(staging, state)
    except BaseException:
        shutil.rmtree(staging, ignore_errors=True)
        raise

    local_host = local_connect_host(args.listen_host)
    smoke_result: dict[str, object] | None = None
    service_path: Path | None = None
    if args.service == "none":
        if not args.no_smoke:
            smoke_result = smoke(state, local_host, args.listen_port,
                                 host, members[0])
    else:
        service_path = install_selected_service(state, args.service,
                                                args.service_name, system)
        try:
            wait_for_listener(None, local_host, args.listen_port,
                              state / "tls/ca.crt", host)
            smoke_result = bootstrap_and_probe(
                state, local_host, args.listen_port, host, members[0])
        except BaseException:
            remove_service(service_path)
            raise

    return {
        "state": str(state),
        "endpoint": endpoint(profile_host, args.port),
        "tls_name": host,
        "listen": endpoint(args.listen_host, args.listen_port),
        "members": members,
        "ca": str(state / "tls/ca.crt"),
        "ca_sha256_fingerprint": fingerprint,
        "handoffs": [str(state / f"secrets/client-{member}.json")
                     for member in members],
        "service": str(service_path) if service_path else None,
        "service_persistence": service_persistence(system, service_path),
        "smoke": smoke_result,
    }


def parser() -> argparse.ArgumentParser:
    value = argparse.ArgumentParser(description=__doc__)
    value.add_argument("state", type=Path,
                       help="new private state directory; existing paths are refused")
    value.add_argument("--host",
                       help="DNS name or IP clients verify in the server certificate")
    value.add_argument("--connect-host",
                       help="optional profile destination when it differs from TLS name")
    value.add_argument("--port", type=int, default=4222,
                       help="port written into member handoffs")
    value.add_argument("--listen-host", default="127.0.0.1",
                       help="local IP literal on which NATS listens")
    value.add_argument("--listen-port", type=int, default=4222)
    value.add_argument("--member", action="append", default=[])
    value.add_argument("--nats-server", type=Path,
                       help="verified local 2.14.3 binary instead of download")
    value.add_argument("--nats-cli", type=Path,
                       help="verified local 0.4.0 binary instead of download")
    value.add_argument("--openssl", default="openssl")
    value.add_argument("--service", choices=("none", "auto", "systemd-user", "launchd"),
                       default="none")
    value.add_argument("--service-name", default="causal-chat")
    value.add_argument("--resume-service", action="store_true",
                       help="install/start a service from existing verified state")
    value.add_argument("--no-smoke", action="store_true",
                       help="skip the default end-to-end temporary service probe")
    return value


def main() -> int:
    args = parser().parse_args()
    try:
        result = provision(args)
    except (ProvisionError, OSError, subprocess.SubprocessError, ssl.SSLError,
            json.JSONDecodeError) as error:
        print(f"provision failed: {error}", file=sys.stderr)
        return 1
    print(json.dumps(result, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
