# Causal Chat for Haiku

`CausalChat` is a native `BApplication` that connects directly to NATS using
Haiku's network library. Rooms are created locally on first join and map to
`chat.room.<name>` subjects. Network callbacks cross into the UI exclusively
through `BMessenger`.

Version 0.4 presents that model as a room workspace rather than a generic
socket form: stable semantic room colors, native message cards, distinct
local/remote messages, bounded two-line previews, and a persistent transport
and history status rail. The visual layer does not invent authority: room
color is presentation, while TLS and authentication remain separately
verified transport properties.

With an operator-provisioned JetStream cursor, the same client provides
server-backed, acknowledged replay. The client is deliberately not allowed to
create consumers; `NATS_CONSUMER` names the durable pull cursor provisioned for
that credential by the [operator bundle](deploy/README.md):

```sh
NATS_TLS=1 NATS_USER=amber NATS_PASSWORD='member secret' \
NATS_JETSTREAM=1 NATS_STREAM=CAUSAL NATS_CONSUMER=amber \
  ./build/CausalChat
```

Replay mode requires verified TLS and simple stream/consumer names. Every
accepted JetStream message is acknowledged after it reaches the UI/history
boundary. A 500-message pull window matches the stream's per-room retention
bound and is renewed without granting consumer-admin subjects to the client.

On a Haiku development installation:

```sh
pkgman install openssl3_devel
sh build-haiku.sh
./build/CausalChat
```

To also produce an installable Haiku package:

```sh
sh build-haiku.sh --package
```

On a small boot volume, keep transient objects and package staging elsewhere:

```sh
CAUSAL_BUILD_DIR=/NPSPACE/causal-build TMPDIR=/NPSPACE/tmp \
  sh build-haiku.sh --package
```

The server and local display petname are configurable:

```sh
NATS_HOST=nonlocal.info NATS_PORT=4222 CAUSAL_PETNAME=amber \
  ./build/CausalChat
```

For a TLS server, certificate-chain and hostname verification are mandatory:

```sh
NATS_HOST=chat.example.org NATS_PORT=4222 NATS_TLS=1 \
  ./build/CausalChat
```

Use a private or test CA explicitly when it is not in the system trust store:

```sh
NATS_TLS=1 NATS_CA_FILE=/path/to/ca.crt ./build/CausalChat
```

`NATS_TLS_NAME` can override the verified DNS name when the TCP endpoint is an
IP address or private proxy. Authentication accepts either `NATS_TOKEN`, or the
pair `NATS_USER` and `NATS_PASSWORD`. Credentials are refused unless
`NATS_TLS=1`; they are never accepted over the plaintext compatibility mode.

The companion diagnostic client can subscribe to several rooms or publish a
single payload:

```sh
./build/nats-chat tail chat.room.lobby chat.room.meta
./build/nats-chat say chat.room.lobby '{"sender":"amber","text":"hello"}'
```

The `nonlocal.info` endpoint currently uses unencrypted NATS transport. Petnames are
presentation labels, not authenticated identities, and direct messages must
not be treated as private until server-side accounts, permissions, and TLS are
configured. The operator requirements and current live-service evidence are in
[`DEPLOYMENT.md`](DEPLOYMENT.md).

## Recovery test

The client keeps requested room subscriptions across disconnects and retries
with bounded exponential backoff. The deterministic fixture can validate that
lifecycle without disrupting a shared NATS server:

```sh
c++ -std=c++17 -O2 -Wall -Wextra -pedantic \
  tests/fake_nats_server.cpp -lnetwork -o build/fake-nats-server

NATS_HOST=127.0.0.1 NATS_PORT=44222 ./build/CausalChat &
./build/fake-nats-server 44222 5
./build/fake-nats-server 44222 30
```

The expected visible sequence is `Connected` → `Offline; retrying` →
`Connected`, without replacing the application team.

Each message emitted by the visual client carries a unique `id`, `sender`, and
`text`. A bounded 4096-entry identity window suppresses duplicate delivery,
and each room retains at most 500 rendered messages. These bounds make future
JetStream replay safe without allowing an unbounded transcript or deduplication
set to exhaust a client.

## Local history

Received messages are restored after restart from:

```text
~/config/settings/CausalChat/history.ndjson
```

The journal tolerates malformed or torn records, inserts a record boundary
after a torn tail, and atomically compacts once it exceeds 8 MiB. In-memory
room and deduplication bounds still apply during restore. This local journal is
a convenience cache, not server-authoritative history.

Disable persistence completely:

```sh
CAUSAL_HISTORY=0 ./build/CausalChat
```

Tests and portable installations can select another journal explicitly:

```sh
CAUSAL_HISTORY_PATH=/volume/private/causal.ndjson ./build/CausalChat
```
