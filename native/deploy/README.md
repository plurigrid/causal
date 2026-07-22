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

## Connect a Haiku member

Each person receives only their own plaintext password, CA path, and durable
consumer name:

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
