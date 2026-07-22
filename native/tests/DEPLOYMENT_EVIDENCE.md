# Deployment evidence — 2026-07-22

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

## Native onboarding extension

1. A native start with no connection environment and no profile opened the
   `Causal Chat - Connect` window. Its public profile saved atomically at mode
   `0600`, handed off to the chat window, and established the public socket.
2. The isolated profile fixture round-tripped every non-secret field, rejected
   plaintext credentials and replay, and proved that a distinctive token was
   absent from the flattened bytes and from the reloaded object.
3. A launch with connection environment variables bypassed setup, established
   its socket, and left the existing saved-profile SHA-256 unchanged.
4. The setup UI populated a local TLS profile, including a private CA and test
   user/password. The native client verified the chain and IP SAN, subscribed,
   rendered the fixture proof message, and labeled both header and status rail
   `TLS`. The mode-`0600` profile contained no fixture password bytes.
5. Restarting that session-only profile restored all non-secret fields but an
   empty secret. An immediate connect remained in setup; entering the password
   once completed a second verified TLS connection.
6. A non-sensitive KeyStore fixture exercised real Haiku authorization prompts
   and passed store, exact retrieval, removal, and negative retrieval. Only
   `Allow once` was granted; five action-specific prompts were observed.
7. The product opt-in path stored a non-sensitive fixture after two one-time
   prompts, reconnected after restart with an empty UI secret after one prompt,
   then removed the remembered entry after two prompts. The profile returned
   to `remember=0` and stayed in setup asking for a session secret.
8. All temporary onboarding profiles and logs, plus every KeyStore fixture
   entry, were removed. The reusable local CA fixture, normal saved public
   profile, and app remained usable.
9. The polished setup rendered both open and TLS-plus-replay states at its
   normal size and at the enforced `900 x 720` minimum without overlapping
   controls. Six subsequent chat resize transitions preserved the window,
   title, room layout and established public socket.
10. Package `causal_chat-0.4.0-3-x86_64.hpkg` reported revision `0.4.0-3` and
    SHA-256 `66e193eeffe3dd3f23748b9e1028d3e40f11b0b64f17284d7dec2ddf4d71570b`.
    Its clean-extracted native binary exactly matched the standalone binary at
    SHA-256 `756ebe788628c14daf32311350effa1e65c01f6c2153986a9e0ebb07b4ec1cf4`,
    and the isolated profile checks passed again against the rebuilt source.
11. The clean-extracted package then exercised durable replay through the
    native setup UI itself against strict NATS 2.14.3 / JetStream API level 4.
    An operator query measured one pending offline message before connection.
    The UI supplied TLS trust, fixture credentials, stream `CAUSAL` and
    consumer `haiku-ui`; the chat rendered the exact marker and reported
    verified TLS plus durable replay. An independent postcondition query
    measured `pending=0` and `ack_pending=0`. The isolated profile remained
    mode `0600` and contained no fixture-password bytes.

## What this does not prove

- `nonlocal.info:4222` still advertises neither TLS nor authentication and is
  not the service described above.
- No public DNS certificate, production storage volume, backup policy, public
  trusted-service deployment, or independent third-party installation has yet
  been observed. Public RC1 and RC2 package downloads exist, but that alone is
  not outside-user validation.
- A display petname is not cryptographic identity.
