# Causal Chat for Haiku

This branch contains a native Haiku multichannel client and the independently
operable NATS service bundle behind it. It is intentionally separated from the
project's default Emacs package surface while the experiment is validated by
people other than its authors.

The release contains:

- a native `BApplication` using Interface Kit, `BMessenger`, libnetwork and
  OpenSSL 3;
- a native first-run connection profile with atomic non-secret persistence,
  session-only secrets by default, and explicitly low-security KeyStore opt-in;
- verified TLS and hostname checks before credentials are transmitted;
- per-user server permissions and operator-owned durable JetStream cursors;
- acknowledged, bounded offline replay and opt-out local history;
- an x86_64 Haiku package plus SHA-256 sidecar;
- a standard-library-only server bootstrap with no checked-in secrets.
- a rootless provisioner that checksum-pins upstream binaries, creates private
  PKI/member handoffs, and refuses success until durable replay is acknowledged.
- a privacy-minimized external-install receipt that exercises the downloaded
  native package end to end without distributing a credential.

Start with [`native/README.md`](native/README.md). Server operators should use
[`native/deploy/README.md`](native/deploy/README.md), and reviewers can inspect
the falsifiable test ledger in
[`native/tests/DEPLOYMENT_EVIDENCE.md`](native/tests/DEPLOYMENT_EVIDENCE.md).

The public `nonlocal.info:4222` service remains an unencrypted compatibility
surface and must not carry secrets. A second endpoint at
`causality-2.pirate-dragon.ts.net:10000` now satisfies the TLS,
authentication, permissions, bounded storage and replay checks, but is
tailnet-only until its owner enables Tailscale Funnel. Its public CA is packaged
under `deploy/trust`; client passwords remain private per-user handoffs.
