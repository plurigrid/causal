# Public deployment contract

The client package is usable today for public, non-secret room traffic. It is
not yet a private messenger.

## Observed service state

A direct probe on 2026-07-21 found:

| Surface | Result |
| --- | --- |
| `nonlocal.info:4222` | Open NATS 2.9.19 |
| NATS `INFO` | No `tls_required` or `auth_required` advertisement |
| `4223`, `4443`, `8222` | Closed |
| JetStream | Not advertised or otherwise proven |

NATS documents that its default server has no authentication or authorization
and is suitable only for development or simple embedded uses. Accounts isolate
subject namespaces, while per-user permissions constrain publish and subscribe
subjects. See the official [server configuration][configuration],
[authentication][authentication], and [authorization][authorization] guides.

## Required production shape

The server operator, not a display petname, must establish authority. A minimal
starting configuration is structurally like:

```conf
listen: 0.0.0.0:4222

tls {
  cert_file: "/run/secrets/server.crt"
  key_file:  "/run/secrets/server.key"
  ca_file:   "/run/secrets/ca.crt"
  handshake_first: true
}

jetstream {
  store_dir: "/var/lib/nats/jetstream"
  max_file: 10G
}

authorization {
  users: [
    {
      user: "alice"
      password: "$ALICE_BCRYPT"
      permissions: {
        publish: ["chat.room.*", "chat.dm.alice.*"]
        subscribe: ["chat.room.*", "chat.dm.alice.>"]
      }
    }
  ]
}
```

This is an operator template, not a secret-bearing checked-in configuration.
Passwords should be bcrypt hashes at rest. Direct-message subject permissions
must be generated per identity; a shared wildcard subscription would destroy
DM confidentiality. NATS documents TLS and optional mutual certificate
verification in its [TLS authentication guide][tls].

JetStream must use dedicated local storage and explicit resource limits. NATS
advises against shared NAS/NFS storage for replicated JetStream state; see its
[JetStream configuration guide][jetstream].

## Client gates

Native Haiku TLS has positive chain/hostname evidence plus negative wrong-host
and untrusted-CA tests. Encrypted reconnect was also verified without replacing
the application team. The checked-in operator bundle now passes all four gates
against a strict local NATS 2.14.3 service:

1. Distinct bcrypt-backed Alice and Bob credentials; no-auth is rejected.
2. Per-user pull subjects; Bob is denied Alice's durable cursor.
3. A bounded `CAUSAL` stream with operator-owned consumers, acknowledged
   offline replay, reconnect cursor continuity, and independent Alice/Bob state.
4. Live roster reload revokes Bob while Alice remains authorized; restoring the
   roster restores Bob without restarting the server.

Exact evidence and remaining limits are recorded in
[`tests/DEPLOYMENT_EVIDENCE.md`](tests/DEPLOYMENT_EVIDENCE.md). These checks prove
the release candidate and deployment procedure, not the current production
state of `nonlocal.info`.

The UI labels the current public connection `PUBLIC`; replay mode refuses to
start without verified TLS. Package documentation continues to describe
petnames as presentation only.

The client's bounded local history alone is not evidence of delivery to another
observer. Version 0.4 can instead use an acknowledged, operator-owned durable
consumer. Reusing one credential across devices shares that cursor; independent
device replay requires independently provisioned consumers.

[configuration]: https://docs.nats.io/running-a-nats-service/configuration
[authentication]: https://docs.nats.io/running-a-nats-service/configuration/securing_nats/auth_intro
[authorization]: https://docs.nats.io/running-a-nats-service/configuration/securing_nats/authorization
[tls]: https://docs.nats.io/running-a-nats-service/configuration/securing_nats/auth_intro/tls_mutual_auth
[jetstream]: https://docs.nats.io/running-a-nats-service/configuration/resource_management
