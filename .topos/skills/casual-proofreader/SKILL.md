---
name: casual-proofreader
description: Transient-based Emacs porcelain for proof assistants (Proof General, Lean 4, Narya). Discoverable keyboard menus for stepping, tactics, goals, holes, and search.
version: 0.1.0
---

# Casual ProofReader

**Trit**: 0 (ERGODIC - coordinator between proof backends)
**Color**: #3A71C0

## Overview

Casual ProofReader provides Transient menus for proof assistants,
solving the "too many keybindings to remember" problem for formal
verification workflows.

## Supported Backends

| Backend | Mode | Detection |
|---------|------|-----------|
| Proof General | `proof-mode` (Coq, Narya PG) | `featurep 'proof-site` |
| Lean 4 | `lean4-mode` | `derived-mode-p` |
| Narya | `narya-mode` | `derived-mode-p` |

## Installation

```elisp
(require 'causal-proofreader)

;; Proof General
(with-eval-after-load 'proof-general
  (keymap-set proof-mode-map "C-o" #'causal-proofreader-tmenu))

;; Lean 4
(with-eval-after-load 'lean4-mode
  (keymap-set lean4-mode-map "C-o" #'causal-proofreader-tmenu))
```

## Menu Structure

```
ProofReader [Backend Name]
├── Proof Navigation: n/p step, RET to-point, b buffer, . locked, r retract
├── Goals & Holes: g goals, c context, h holes›, t tactics›
├── Search: s lemma, / isearch, i imenu
├── Window: w layout›
└── Settings: , settings›
```

## Tactics Sub-menu

```
Introduce & Eliminate: i intro, a apply, e exact, c cases, I induction
Simplify & Solve: s simp, r ring, o omega, = rfl, w rewrite
```

## MCP Integration: Mnemosyne

Proof transcripts can be persisted to RDF knowledge graphs via
[sophia-labs/mnemosyne-mcp](https://github.com/sophia-labs/mnemosyne-mcp):

- Proof sessions → Mnemosyne graphs
- Tactic sequences → document blocks
- Lemma dependencies → semantic wires

## GF(3) Triad

```
casual-proofreader (0) ⊗ proofgeneral-narya (-1) ⊗ lean-proof-walk (+1) = 0 ✓
```

## Related Skills

- `proofgeneral-narya` (-1): PG + Narya integration
- `lean-proof-walk` (+1): Lean 4 state chains
- `narya-hatchery` (0): Narya proof assistant
- `holes` (0): Typed holes
- `elisp`: Emacs Lisp
- `emacs`: Emacs ecosystem

## Source

`~/i/causal/lisp/causal-proofreader.el`
