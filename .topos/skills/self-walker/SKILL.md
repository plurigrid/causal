---
name: self-walker
description: Autonomous walker over causal-proofreader + ProofGeneral/Narya: reads its own Transient menu tree, steps through proof states, and emits a state chain ending in a CatColab olog snapshot.
version: 0.1.0
metadata:
  trit: 0
  source: plurigrid/causal + ProofGeneral/PG + mikeshulman/narya
---

# Self-Walker: causal/proofgeneral

> *"The proof assistant that reads its own menu tree and walks."*

**Trit**: 0 (ERGODIC — coordinates between generation and verification)

## Overview

Self-walker is an introspective automation for the `causal-proofreader` + ProofGeneral/Narya
stack. It operates by:

1. Reading `causal-proofreader.el` to discover available tactic/navigation commands
2. Connecting to a live ProofGeneral session (Coq or Narya backend)
3. Walking proof states step-by-step via `causal-proofreader` Transient commands
4. Recording each `Γ ⊢ G` transition as a state-chain entry
5. Exporting the terminal proof state as a CatColab `simple-olog` via `causal-catcolab`

The "self" in self-walker: the walker reads the causal repo's own Elisp source to populate
its tactic vocabulary — no hard-coded tactic list.

## GF(3) Triad

```
self-walker (0) ⊗ proofgeneral-narya (-1) ⊗ lean-proof-walk (+1) = 0 ✓
```

| Trit | Skill | Role |
|------|-------|------|
| +1 | lean-proof-walk | Generates next tactic proposal |
| 0 | **self-walker** | Coordinates: executes via PG, records state |
| -1 | proofgeneral-narya | Verifies: PG rejects unsound steps |

## State Chain Format

Identical to `lean-proof-walk` but driven through ProofGeneral's ISAR/Narya backend:

```
State 0: α : Type, f g : α → α, h : f = g ⊢ f ∘ g = g ∘ f

  tactic: intro x
  via: causal-proofreader-step-forward (C-c C-n in PG mode)

State 1: α : Type, f g : α → α, h : f = g, x : α ⊢ f (g x) = g (f x)

  tactic: rewrite h
  via: causal-proofreader-tactic-rewrite → insert "rewrite h" → step-forward

State 2: No Goals — QED
```

## Protocol

### 1. Introspect

```elisp
;; Discover tactics by reading causal-proofreader.el
(defun self-walker--discover-tactics ()
  "Scan causal-proofreader.el for tactic insertion commands."
  (let ((file (locate-library "causal-proofreader")))
    (with-temp-buffer
      (insert-file-contents file)
      (let (tactics)
        (while (re-search-forward
                "(causal-proofreader--insert-tactic \"\\([^\"]+\\)\")" nil t)
          (push (match-string 1) tactics))
        (nreverse tactics)))))
;; → ("intro" "apply" "exact" "rewrite" "simp" "ring" "omega" "cases" "induction" "rfl")
```

### 2. Initialize

```
backend  := causal-proofreader--dispatch → detect (proof-general | narya | lean)
goal     := causal-catcolab--scan-goal   (reads *Goals* buffer)
hyps     := causal-catcolab--scan-hypotheses
state[0] := (hyps, goal)
seed     := sxhash(goal) ^ 0x9E3779B97F4A7C15
```

### 3. Walk Step

```
loop until state[n] = "No Goals":
  tactic  := lean-proof-walk.propose(state[n], seed)   ; +1 agent
  execute := causal-proofreader-step-forward()           ; 0 agent (self)
  verify  := proof-general-accepted? || narya-accepted?  ; -1 agent
  if rejected:
    tactic := next-tactic-in-vocab(seed)
    retry  (max 3 times, then backtrack via step-backward)
  state[n+1] := causal-catcolab--scan-hypotheses + scan-goal
  seed       := seed XOR sxhash(state[n+1])
```

### 4. Export

```elisp
;; On QED, export proof context as CatColab olog
(causal-catcolab-save-proof-as-olog)
;; Opens browser to catcolab.org/app/new/<uuid>
```

## Invocation

From `causal-proofreader-tmenu` (`C-o` in PG/Narya buffer):

```
C-o  → ProofReader [narya]
  C  → CatColab›
    w  → Self-walk current proof
```

Or directly:

```elisp
M-x self-walker-causal-proofgeneral-run
```

## Output

1. **State chain** — `*self-walker: <buffer>*` buffer showing each `Γ ⊢ G` with tactic
2. **Walk log** — seed evolution, tactic attempts, backtrack count
3. **CatColab olog** — hypothesis types → olog objects, opened in browser
4. **GF(3) walk hash** — Möbius-product of tactic trits along the path

## Files

- `lisp/causal-proofreader.el` — Transient UI, tactic vocabulary source
- `lisp/causal-catcolab.el` — Olog export, state scan
- `lisp/causal-catcolab-utils.el` — JSON-RPC, `causal-catcolab--scan-*`

## Related Skills

- `proofgeneral-narya` (-1): PG/Narya backend, bridge types, 27-agent hierarchy
- `lean-proof-walk` (+1): State-chain protocol, tactic proposal, GF(3) seed
- `causal-proofreader` (0): Transient UI this walker drives
- `catcolab` (-1): Olog destination for exported proof state
