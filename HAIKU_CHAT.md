# Causal Chat for Haiku

This branch contains a native Haiku multichannel client and the independently
operable NATS service bundle behind it. It is intentionally separated from the
project's default Emacs package surface while the experiment is validated by
people other than its authors.

The release contains:

- a native `BApplication` using Interface Kit, `BMessenger`, libnetwork and
  OpenSSL 3;
- verified TLS and hostname checks before credentials are transmitted;
- per-user server permissions and operator-owned durable JetStream cursors;
- acknowledged, bounded offline replay and opt-out local history;
- an x86_64 Haiku package plus SHA-256 sidecar;
- a standard-library-only server bootstrap with no checked-in secrets.

Start with [`native/README.md`](native/README.md). Server operators should use
[`native/deploy/README.md`](native/deploy/README.md), and reviewers can inspect
the falsifiable test ledger in
[`native/tests/DEPLOYMENT_EVIDENCE.md`](native/tests/DEPLOYMENT_EVIDENCE.md).

The public `nonlocal.info:4222` service remains an unencrypted compatibility
surface and must not carry secrets. A production deployment is only equivalent
to the tested system when it satisfies the TLS, authentication, permissions,
bounded storage, replay and revocation checks in the ledger.
