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

## Visual rebuild candidate

1. Revision `0.4.0-4` compiled on the native Haiku host without warnings and
   its connection-profile fixture passed. The HPKG reported the matching
   revision and retained the same Haiku, OpenSSL 3 and license metadata.
2. The standalone and clean-extracted package binaries matched byte-for-byte
   at SHA-256 `6381a5af0dbcf3fba1489aede166bce915a2f28e5bcd4fbcc6ff866ab5891e43`.
   The final HPKG, rebuilt to include the public CA and dependency-free operator
   probes, had SHA-256
   `0e20a2d6870946e429e7ccb8c397f3b8adf8882e1bafd786806be34265201fc7`.
3. Native screenshots witnessed the semantic room header, remote/local signal
   cards, causal field and composer at both the normal frame and the enforced
   `820 x 560` minimum. The first screenshot exposed an off-screen default
   frame; a screen-aware centered frame corrected it before this evidence was
   accepted. The clean-package normal screenshot SHA-256 was
   `bfac932112e1e79231277be9776f09f26c4fdf23ddeae284238ac3c3f23e3806`;
   the minimum-frame screenshot SHA-256 was
   `542341db015f8e02eb72c5e96e3e8fcf1c26ce1bee7d5c76149c5c52d9167c41`.
4. Six scripted minimum, wide and tall frame transitions preserved one Haiku
   application team. After the trajectory, the same team retained an
   established socket to `nonlocal.info:4222`.
5. The clean-extracted HPKG binary replaced the standalone validation process,
   opened visibly as team `11990`, and established the public socket. The
   public endpoint remains plaintext; the UI therefore continued to label it
   `OPEN / PLAINTEXT` rather than implying transport authority.

## Independent staged-service extension

1. The official NATS Server 2.14.3 Darwin archive was re-used only after its
   SHA-256 matched the upstream `SHA256SUMS`. The stable binary retained version
   2.14.3 and was installed outside the repository.
2. The server binds only `127.0.0.1:44524`; Tailscale Serve carries raw TCP on
   tailnet port `10000`. NATS itself terminates TLS 1.3 with hostname-checked
   certificate `causality-2.pirate-dragon.ts.net`. The tracked CA certificate
   matched the live trust anchor at fingerprint
   `C2:57:DD:C9:C1:DD:D7:48:9E:07:A6:54:45:B0:7E:54:6D:9B:86:A4:20:4D:FE:CE:AE:49:79:10:B6:AC:05:0E`.
   Its PEM file SHA-256 is
   `331405fb79800d44645ec1dd83972b0ece5e89ab874718d3cad3b2fb2ae6c639`.
3. Three randomly generated per-member credentials exist only as mode-`0600`
   private handoffs. The server consumes a bcrypt-only roster. Anonymous access
   was rejected and `member-b` was denied `member-a`'s pull subject.
4. Two independently routed Linux hosts used different credentials and the
   packaged CA to complete exact room publish/subscribe roundtrips through
   `100.69.33.107:10000`. Their temporary credential copies were removed.
5. A user `launchd` service restarted NATS, restored five stream messages and
   all three cursors, and retained a loopback-only listener. A post-restart
   authenticated roundtrip passed.
6. The clean-extracted Haiku package connected through verified TLS as
   `member-b`, rendered all five offline signals, and acknowledged them. An
   operator postcondition measured `member-b pending=0, ack_pending=0`, while
   `member-a` and `guest` independently remained at `pending=5`.
7. The accepted native screenshot SHA-256 is
   `3bb584693a95ca6dfdcce489535695a382a924b57f512ec2592bb7242db10e15`.
   A prior black frame was rejected because it showed the screen blanker rather
   than the application.

## Independent rootless-operator extension

1. A fresh aarch64 Linux host used `provision_server.py` to fetch the pinned
   NATS Server 2.14.3 and NATS CLI 0.4.0 assets, create new RSA-3072 private PKI,
   generate two unrelated member handoffs, start a user systemd service, and
   complete its default durable publish/pull/ack smoke test. No root-owned
   package installation or Docker container carries the service.
2. The first user-service sandbox failed closed because that host cannot apply
   `PrivateDevices` or `ProtectKernelModules` in its user manager. The unit was
   rolled back while state remained mode `0700`. Removing only those two
   unsupported directives preserved `NoNewPrivileges`, `PrivateTmp`, strict
   system/home protection, kernel-tunable/control-group protection, SUID/SGID
   and personality restrictions, and an AF_UNIX/INET/INET6 allowlist.
3. An intermediate handoff explicitly separated `host=100.107.33.61` from
   `tls_name=c124b1.pirate-dragon.ts.net`. This repaired two real negative
   controls: ZeroTier carried the TCP handshake but stalled the RSA certificate
   flight, while MagicDNS was not configured on the client. A later probe also
   showed that Tailscale's netfilter chain admitted the port before the proposed
   peer-specific UFW rule, so that route was rejected as the final perimeter.
   Transport reachability was never renamed certificate authority or peer
   restriction.
4. A second aarch64 Linux machine consumed `external-a` through verified TLS.
   The service then restarted with a different PID while retaining two stream
   messages and independent cursor state. `external-b` subsequently
   acknowledged three messages; after the transferred source directory was
   removed and the service restarted again, `external-a` acknowledged its two
   pending messages using the probe copied from the installed state itself.
5. The final service binds only `10.7.0.1:44525` on the dedicated two-host
   cluster link. `10.7.0.3` timed out while the live listener existed, then
   connected only after UFW admitted that source, destination, interface and
   TCP port. Anonymous authentication and `external-b` access to `external-a`'s
   pull subject were both rejected. Every transient profile/CA copy was deleted;
   the authoritative mode-`0600` handoffs remain only in the operator state.
6. The user service is enabled and Linux lingering is enabled for its account.
   Its self-contained `tools/` directory replaced the transferred source as the
   management surface.
7. Revision candidate `0.4.0-5` rebuilt on the native x86_64 Haiku host at
   `hrev59861` with GCC 13.3.0 and OpenSSL 3.5.7 headers. Because `/boot` had
   only 504 KiB free, source, temporary files and package staging remained on
   `/NPSPACE`; a build-local linker view used the installed versioned OpenSSL
   runtime without mutating the boot volume. The native connection-profile
   fixture passed.
8. The standalone and clean-extracted package executables were byte-identical
   at SHA-256
   `6381a5af0dbcf3fba1489aede166bce915a2f28e5bcd4fbcc6ff866ab5891e43`.
   The package reports `0.4.0-5`, x86_64, Haiku, OpenSSL 3 and GPLv3 metadata,
   and includes the five self-contained operator tools.
9. The exact extracted executable rendered three deterministic observer cards
   from a loopback fixture. Six minimum, wide and tall frame transitions kept
   the same application team (`13351`) and established socket. The final
   1011 by 735 active-window screenshot SHA-256 is
   `bb73b378f04b10e2ebb01a21e7d8f3118e6996b1dfa8bad4b3205374ddcc8f2f`.

## What this does not prove

- `nonlocal.info:4222` still advertises neither TLS nor authentication and is
  not the service described above.
- The staged service has persistent storage and independent-machine evidence,
  but remains tailnet-only. Public Funnel activation and an independently
  operated third-party installation have not yet been observed.
- The rootless extension proves independent machine installation, restart,
  cursors and transport. Both machines were still operated within this
  investigation; this is not evidence of an unaffiliated human installation.
- A display petname is not cryptographic identity.
