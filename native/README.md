# Causal Chat for Haiku

`CausalChat` is a native `BApplication` that connects directly to NATS using
Haiku's network library. Rooms are created locally on first join and map to
`chat.room.<name>` subjects. Network callbacks cross into the UI exclusively
through `BMessenger`.

Version 0.4 presents that model as a room workspace rather than a generic
socket form: stable semantic room colors, native message cards, distinct
local/remote messages, bounded adaptive previews, a low-cost causal field, and
a persistent transport and history status rail. The window is centered against
the live Haiku screen frame so its composer is visible on first launch, and its
cards remain legible at the enforced minimum size. The visual layer does not
invent authority: room color is presentation, while TLS and authentication
remain separately verified transport properties.

## First run and credentials

Without connection environment variables, the app opens a native connection
profile window. It saves the non-secret endpoint, TLS, replay, display-name and
history choices to `~/config/settings/CausalChat/profile` using an atomic
replacement and mode `0600`. Passwords and tokens are never flattened into
that file.

Secrets are session-only by default. The optional **Remember in Haiku
KeyStore** checkbox uses a dedicated `CausalChat` keyring from a worker thread,
so the system permission prompt cannot freeze the window. This option is
explicitly labeled low-security: Haiku R1's own documentation says KeyStore is
permission-gated but unencrypted on disk. Unchecking it removes the prior key;
the app then asks for the secret once per session.

The setup window enforces the same transport gates as the network client:
credentials and durable replay are rejected on plaintext. A blank `USER`
means the secret field is a token; a nonblank `USER` makes it a password.

Environment variables remain the highest-priority automation and recovery
surface. If any connection variable is present, the app bypasses the profile
window and does not rewrite the saved profile. Tests can isolate profiles with
`CAUSAL_PROFILE_PATH=/path/to/profile`.

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

## Trusted staging endpoint

The measured staging service is available to authorized tailnet peers at
`causality-2.pirate-dragon.ts.net:10000`. It uses end-to-end TLS terminated by
NATS, a bounded `CAUSAL` stream, and a distinct credential/cursor per member.
Use these setup fields with the private handoff from the operator:

```text
HOST       causality-2.pirate-dragon.ts.net
PORT       10000
TLS        on
TLS NAME   causality-2.pirate-dragon.ts.net
CA FILE    /boot/system/apps/CausalChat/deploy/trust/causal-chat-ca-v1.crt
STREAM     CAUSAL
CONSUMER   value from the member handoff
```

The CA certificate is public; its expected SHA-256 fingerprint is
`C2:57:DD:C9:C1:DD:D7:48:9E:07:A6:54:45:B0:7E:54:6D:9B:86:A4:20:4D:FE:CE:AE:49:79:10:B6:AC:05:0E`.
The password is not in the package and should remain session-only. This address
is not yet generally internet-reachable: the owner must enable Funnel before a
person outside the tailnet can use the same endpoint.

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

If the matching `openssl3_devel` package has been extracted onto that larger
volume instead of activated through packagefs, point the build at its root.
The script uses those headers and creates a build-local linker view of the
installed versioned OpenSSL 3 runtime; it does not mutate `/boot`:

```sh
OPENSSL3_DEVEL_ROOT=/NPSPACE/causal-deps/openssl3-devel \
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

## Independent public receipt

An outside Haiku tester can validate a downloaded RC6 without receiving any
credential. Download the HPKG and checksum sidecar from the release, extract
the expected hash, and run the packaged check:

```sh
expected=$(awk '{print $1}' causal_chat-0.4.0-6-x86_64.hpkg.sha256)
receipt_bundle=$(mktemp -d "${TMPDIR:-/tmp}/causal-rc6-bundle.XXXXXX")
package extract -C "$receipt_bundle" causal_chat-0.4.0-6-x86_64.hpkg
sh "$receipt_bundle/apps/CausalChat/external-install-check.sh" \
  causal_chat-0.4.0-6-x86_64.hpkg "$expected"
```

If `/tmp` is on a small boot volume, set and export a spacious `TMPDIR`, for
example `/NPSPACE/tmp`, before these commands. The evidence directory is
reported separately on stderr for local inspection and is not part of the
pasteable receipt.

The check verifies package checksum and metadata, clean-extracts the native programs,
launches the exact `BApplication`, publishes a unique non-sensitive marker,
observes it after the UI/history boundary, checks deduplication, quits cleanly,
and prints a receipt containing hashes and coarse OS facts but no hostname,
username, address, local path, or message body. It deliberately uses the public
plaintext compatibility room and therefore proves distribution and basic
multiplayer function—not the separate trusted-service claims.

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

The profile and optional KeyStore boundaries have native fixtures:

```sh
c++ -std=c++17 -O2 -Wall -Wextra -pedantic \
  tests/connection_profile_test.cpp connection_profile.cpp -lbe \
  -o build/connection-profile-test
build/connection-profile-test /tmp/causal-profile-test/profile

c++ -std=c++17 -O2 -Wall -Wextra -pedantic \
  tests/connection_keystore_test.cpp connection_profile.cpp -lbe \
  -o build/connection-keystore-test
build/connection-keystore-test
```

The second test uses only a disposable, non-sensitive fixture and deliberately
exercises Haiku's real keyring authorization prompt. It removes the key before
returning success.

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
