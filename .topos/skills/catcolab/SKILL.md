---
name: catcolab
description: Transient UI for CatColab — Topos Institute's collaborative categorical modeling environment. Browse, create, and export ologs, causal loops, and Petri nets from Emacs.
version: 0.1.0
---

# CatColab — Categorical Modeling from Emacs

**Trit**: -1 (MINUS — consumes/queries external categorical models)
**Source**: [ToposInstitute/CatColab](https://github.com/ToposInstitute/CatColab)

## Overview

This skill provides an Emacs porcelain for [catcolab.org](https://catcolab.org) via
its JSON-RPC 2.0 backend API. It integrates with `causal-proofreader` so proof
states can be exported as ologs (ontology logs) — the categorical modeling
primitive for "things and aspects."

## Files

- `lisp/causal-catcolab.el` — Transient menus, commands, proof-to-olog export
- `lisp/causal-catcolab-utils.el` — JSON-RPC 2.0 client, model constructors, display helpers

## Quick Start

```elisp
(require 'causal-catcolab)
(keymap-set global-map "C-c C" #'causal-catcolab-tmenu)
```

## Wire Format

```
POST <backend>/rpc
Content-Type: application/json

{"jsonrpc":"2.0","method":"<name>","params":[<args>],"id":<n>}

Response:
  {"jsonrpc":"2.0","result":{"tag":"Ok","content":<data>},"id":<n>}
  {"jsonrpc":"2.0","result":{"tag":"Err","code":<n>,"message":"..."},"id":<n>}
```

## Supported Model Types

From the catlog stdlib:

| Theory ID | Description |
|---|---|
| `simple-olog` | Ontology log (objects + aspects) |
| `simple-schema` | Database schema |
| `causal-loop` | Causal loop diagram |
| `causal-loop-delays` | Causal loop with delay annotations |
| `indeterminate-causal-loop` | Indeterminate causal loop |
| `petri-net` | Petri net |
| `reg-net` | Regulatory network |
| `primitive-stock-flow` | Stock-flow diagram |
| `power-system` | Power system model |

## Transient Menu Structure

```
causal-catcolab-tmenu
├── Browse
│   ├── b  Browse public docs
│   ├── s  Search by name
│   ├── v  View ref by UUID
│   └── o  Open in browser
├── Create
│   ├── n› New submenu
│   │   ├── o  Simple Olog
│   │   ├── c  Causal Loop Diagram
│   │   ├── p  Petri Net
│   │   └── P  Proof → Olog
│   └── P  Proof → Olog (shortcut)
└── Meta
    ├── ?  Status check
    └── ,› Settings
        ├── u  Backend URL
        ├── t  Auth token
        ├── l  Use local (localhost:8000)
        └── r  Use remote (backend.catcolab.org)
```

## Proof → Olog Pipeline

The `causal-catcolab-save-proof-as-olog` command bridges proof assistants
and categorical modeling:

1. Reads hypotheses from `*Goals*` buffer (Lean 4 / Proof General) or current region
2. Parses `name : Type` lines into (name . type) pairs
3. Maps hypotheses → olog objects (ObType), goal → distinguished object
4. Creates a `simple-olog` document via CatColab RPC
5. Opens the resulting olog in the browser for collaborative editing

## Integration Points

- **causal-proofreader**: Export proof context as categorical model
- **sophia-mnemosyne**: Wire olog refs into the RDF knowledge graph
- **mnemosyne-mcp**: All CLIs can query CatColab documents stored as Mnemosyne wires

## Configuration

```elisp
;; Remote (default — no auth needed for public docs)
(setq causal-catcolab-url "https://backend.catcolab.org")

;; Local development instance
(setq causal-catcolab-url "http://localhost:8000")

;; Authenticated access (Firebase Bearer token)
(setq causal-catcolab-token "your-firebase-token")
```

## GF(3) Triad

```
catcolab (-1) ⊗ causal-proofreader (0) ⊗ sophia-emacs (+1) = 0
```

## Related Skills

- `causal-proofreader` (0): Proof navigation, source of proof states
- `sophia-emacs` (+1): RDF knowledge graph, stores olog references as wires
- `proofreader-interleave` (0): Outside-in membrane connecting periphery to core
