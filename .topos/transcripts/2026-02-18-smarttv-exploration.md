# SmartTV 4K Exploration — bc:5c:17:7a:08:04

**Date**: 2026-02-18
**Seed**: 207103716886532 (MAC as integer)
**Color**: #1FB60A (vivid green)

## Device Profile

| Field | Value |
|-------|-------|
| Name | SmartTV 4K |
| Manufacturer | Hisense (OUI: Qingdao Intelligent & Precise Electronics) |
| MAC | bc:5c:17:7a:08:04 |
| IP | 10.0.0.61 |
| Cast Version | 3.72.446070 |
| Build | 446070 |
| SoC | Mediatek T32 ("songshan") |
| API Version | 12 |
| UDN | 6321b049-fd98-931a-d603-b341ca828e71 |
| UMA Client | 36435cd6-f02a-4f56-a36c-f8ee0f1a84f0 |
| Locale | en-US |
| Uptime | ~366 hours (15.3 days) |
| Setup State | 60 (complete) |
| Connected | true (WiFi) |
| Ethernet | false |
| Has Update | false |
| OpenCast | false |

## Open Ports

| Port | Protocol | Service |
|------|----------|---------|
| 8008 | HTTP | DIAL / SSDP device description |
| 8009 | TLS | CastV2 protobuf (gRPC-like) |
| 8443 | HTTPS | Cast setup API (eureka_info) |
| 9000 | ? | Unknown (connection reset) |
| 9080 | HTTP | Health endpoint ("status=ok") |

## TLS Certificate Chain

```
Subject:  CN=9050817776691318073 / Widevine
Issuer:   CN=Hisense TV songshan Mediatek t32 Cast ICA / Widevine
Chain:    → Widevine Cast Subroot → Cast Root CA (Google)
Protocol: TLSv1.2
Cipher:   ECDHE-RSA-CHACHA20-POLY1305
```

## DIAL Apps

| App | State |
|-----|-------|
| ChromeCast | stopped |
| Netflix | stopped |

## mDNS Discovery

```
_googlecast._tcp.local:
  1. Q80F_32-8c77d64d979d3d180e694ff47d94da0e  (another Cast device)
  2. SmartTV-4K-6321b049fd98931ad603b341ca828e71  (this device)
```

## Local Network

| IP | MAC | Role |
|----|-----|------|
| 10.0.0.1 | 40:75:c3:c5:62:a0 | Gateway |
| 10.0.0.51 | 28:94:01:b3:52:14 | Unknown device |
| 10.0.0.61 | bc:5c:17:7a:08:04 | **SmartTV 4K** (this device) |
| 10.0.0.142 | 0a:53:30:23:aa:4c | Unknown device |
| 10.0.0.152 | 0a:1a:ab:7a:36:03 | **Us** (macOS) |
| 10.0.0.155 | 94:e6:ba:07:73:7f | Unknown device |

## 17-Skill Color Map (seed: 207103716886532)

| # | Hex | Skill | Applied |
|---|-----|-------|---------|
| 1 | #1FB60A | network | ARP, port scan, mDNS |
| 2 | #A62C19 | tailscale | mesh bridging (future) |
| 3 | #E81548 | reverse-engineering | TLS cert chain analysis |
| 4 | #F66AE0 | gay-mcp | MAC→seed→color |
| 5 | #613FD0 | nrf5340-device-interaction | hardware protocols |
| 6 | #BE27B0 | iroh-p2p | P2P comms (future) |
| 7 | #3EC372 | sophia-emacs | knowledge graph storage |
| 8 | #E26EDA | mcp-builder | Cast MCP tools (future) |
| 9 | #59EC5F | terminal | TUI probing |
| 10 | #37AD60 | ffmpeg | media casting (future) |
| 11 | #A62FB9 | captp | capability-secure access |
| 12 | #6F69D4 | security | TLS validation |
| 13 | #3ECAE5 | crdt | shared state |
| 14 | #4CC45B | osm-topology | network mapping |
| 15 | #117CAC | localsend-mcp | file transfer |
| 16 | #72A61D | babashka | orchestration |
| 17 | #8D239A | deepwiki-mcp | docs lookup |
