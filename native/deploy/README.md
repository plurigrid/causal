# Causal NATS operator bundle

This directory turns the development fixture into a reproducible service with
four boundaries that the public endpoint does not currently provide:

- TLS is mandatory before NATS sends its greeting.
- Every member has a distinct bcrypt-backed credential and subject ACL.
- `CAUSAL` stores at most 500 messages per room, 50,000 messages or 512 MiB
  overall, and 30 days of history, whichever limit is reached first.
- The operator owns one durable pull consumer per member. Clients can pull and
  acknowledge only their own cursor; they cannot create arbitrary consumers.

The scripts use only Python's standard library. `bootstrap.py` speaks the NATS
and JetStream wire protocols directly so a second client SDK is not part of the
trusted deployment path.

## Provision

Install current `nats-server` and the `nats` CLI from their signed or
checksum-verified upstream releases. Obtain a certificate whose SAN covers the
public DNS name. Keep its private key and every generated file below outside
the repository.

Create password hashes interactively; retain the original secret only for the
person who will use it:

```sh
nats server passwd
```

Copy `users.example.json` to a private path, replace every placeholder with a
complete bcrypt result, and render the server include:

```sh
python3 render_users.py /run/secrets/causal-users.json ./users.conf
```

`users.conf` is written atomically with mode `0600`. It is intentionally
ignored by Git and should remain readable only by the NATS service account.

Start the service from this directory so the relative include resolves:

```sh
CAUSAL_LISTEN=0.0.0.0:4222 \
CAUSAL_TLS_CERT=/run/secrets/fullchain.pem \
CAUSAL_TLS_KEY=/run/secrets/privkey.pem \
CAUSAL_TLS_CA=/run/secrets/chain.pem \
CAUSAL_STORE_DIR=/var/lib/nats/jetstream \
  nats-server -c server.conf
```

Reconcile the bounded stream and every operator-owned consumer. Supply the
administrator password through a transient environment variable or omit it to
receive a terminal prompt:

```sh
CAUSAL_ADMIN_USER=causal-operator \
CAUSAL_ADMIN_PASSWORD='the separately retained secret' \
python3 bootstrap.py /run/secrets/causal-users.json \
  --host chat.example.org --tls-name chat.example.org \
  --ca /run/secrets/chain.pem
```

The same command is idempotent. Add `--check` for a read-only readiness probe.
Its output contains counts and consumer names, never passwords or hashes.

### TLS-terminating Funnel deployment

`server-funnel.conf` is a deliberately separate topology for a local trusted
proxy that terminates public TLS. It contains no NATS TLS block and must never
listen on a LAN or public interface. Bind it to loopback, then expose it with a
TLS-terminated TCP Funnel:

```sh
CAUSAL_LISTEN=127.0.0.1:44524 \
CAUSAL_STORE_DIR=/private/causal/store \
  nats-server -c server-funnel.conf

tailscale funnel --bg --tls-terminated-tcp=10000 \
  tcp://127.0.0.1:44524
```

The client verifies the public `.ts.net` certificate while the only plaintext
segment is kernel loopback between Funnel and NATS. Do not describe this as
process-terminated TLS. `generate_roster.py` can create independent credentials
without printing their plaintext values:

```sh
python3 generate_roster.py /private/causal/secrets \
  --nats-cli /path/to/nats \
  --host node.example.ts.net --port 10000 \
  --member amber --member violet
python3 render_users.py /private/causal/secrets/roster.json ./users.conf
```

Every `client-USER.json` is a mode-`0600` handoff containing one credential.
Transfer it through a separately authenticated private channel, then delete the
recipient copy after onboarding. The server consumes only `roster.json` hashes
and the rendered `users.conf`.

For end-to-end TLS behind a raw TCP proxy, use `server.conf` and distribute only
the public trust anchor. This staging deployment's anchor is
`trust/causal-chat-ca-v1.crt`; its private key is never stored in the repository.
Verify its SHA-256 fingerprint before importing it. `member_probe.py` is a
dependency-free cross-platform check of the same TLS, authentication and room
roundtrip used by the native client:

```sh
python3 member_probe.py client-amber.json \
  --ca trust/causal-chat-ca-v1.crt
```

## Connect a Haiku member

Each person receives only their own plaintext password, CA path, and durable
consumer name. Launching `CausalChat` normally opens the native profile window;
enter the server, TLS name, CA file, member user/password, `CAUSAL` stream and
that member's consumer. Leave **Remember in Haiku KeyStore** off for a
session-only password. Its opt-in storage is permission-gated but unencrypted
on disk in Haiku R1.

Environment variables provide the equivalent automation path:

```sh
NATS_HOST=chat.example.org NATS_PORT=4222 \
NATS_TLS=1 NATS_TLS_NAME=chat.example.org \
NATS_CA_FILE=/boot/home/config/settings/CausalChat/ca.pem \
NATS_USER=amber NATS_PASSWORD='member secret' \
NATS_JETSTREAM=1 NATS_STREAM=CAUSAL NATS_CONSUMER=amber \
CAUSAL_PETNAME=amber /boot/system/apps/CausalChat/CausalChat
```

The display petname is still presentation. The server credential and its
consumer ACL are the authority. Reusing one credential on multiple devices
shares one cursor; provision a distinct member identity when independent
device replay is required.

## Rotate or revoke

Replace or remove the member in the private roster, rerun `render_users.py`,
then ask the server to reload its configuration:

```sh
nats-server --signal reload
```

Verify that the old credential is rejected before issuing the replacement.
Removing a user does not delete their durable consumer or retained room data;
consumer deletion is a separate operator decision.
