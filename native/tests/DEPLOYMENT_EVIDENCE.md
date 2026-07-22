# Deployment evidence — 2026-07-21

This ledger distinguishes the tested release candidate from the unchanged
public endpoint. Test identities and password hashes in this directory are
fixtures only.

## Surface under test

- NATS Server 2.14.3, strict JetStream API level 4.
- TLS 1.2 minimum, handshake-first, certificate and hostname verification.
- Generated bcrypt authorization include with administrator, Alice and Bob.
- Native Haiku R1 beta5 development client, x86_64.
- `CAUSAL`: limits retention, file storage, 500 messages per subject, 50,000
  total messages, 512 MiB, 30 days, 64 KiB per message.
- Durable pull consumers `alice` and `bob`, explicit acknowledgements and a
  maximum 500 pending acknowledgements.

## Measured sequence

1. `nats-server -t` accepted `deploy/server.conf` plus the rendered include.
2. `bootstrap.py` created the stream and both consumers; a second reconciliation
   and `--check` returned the same zero-message state.
3. Bob published three room messages while the Haiku application was offline.
   Readiness then reported `pending=3` independently for Alice and Bob.
4. Alice's native client connected through verified TLS, materialized exactly
   three NDJSON records, acknowledged them, and reached `pending=0` while Bob
   remained at `pending=3`.
5. After Alice quit, Bob published a fourth message. A fresh Alice history
   received exactly that fourth message; a fresh Bob history received all four.
6. Both consumers then reported `pending=0`, `ack_pending=0`; the stream retained
   four messages and 622 bytes.
7. Bob's attempt to publish a pull request to Alice's consumer produced a NATS
   permissions violation. A connection without credentials produced an
   authorization violation.
8. Rendering a roster without Bob and sending SIGHUP made Bob's old credential
   fail while Alice still measured a successful RTT. Restoring the full roster
   and reloading restored Bob at 73 microseconds without restarting the server.
9. Each generated Haiku team exited cleanly through `BApplication` messaging.
10. The clean-extracted `0.4.0-1` package received and acknowledged a fifth
    offline message. Alice returned to `pending=0`; Bob independently retained
    `pending=1`. The package SHA-256 matched before and after transfer.

## What this does not prove

- `nonlocal.info:4222` still advertises neither TLS nor authentication and is
  not the service described above.
- No public DNS certificate, production storage volume, backup policy, public
  package release or independent third-party installation has yet been observed.
- A display petname is not cryptographic identity.
