---
name: proofreader-interleave
description: Outside-in interleave skill connecting hop-2 periphery directly to core 17, making the hop-1 middle layer a naturally traversed membrane. One-reach to ~600 skills.
version: 0.1.0
---

# ProofReader Interleave

**Trit**: 0 (ERGODIC — the membrane itself)
**Color**: #7B68EE (medium slate blue — between warm core and cool periphery)
**Principle**: Connect outside to inside; the middle finds itself.

## Architecture

```
         HOP-2 PERIPHERY (~600 skills)
         ╔═══════════════════════════╗
         ║  directly referenced here ║
         ╚════════════╤══════════════╝
                      │
              ┌───────┴───────┐
              │  THIS SKILL   │ ← you are here
              │  (membrane)   │
              └───────┬───────┘
                      │
         ╔════════════╧══════════════╗
         ║     CORE 17 HUBS         ║
         ║  directly referenced here ║
         ╚═══════════════════════════╝

The hop-1 middle (~95-250 skills) needs no explicit listing.
Every hop-1 skill is reachable because it neighbors both
a core hub AND a hop-2 leaf. The membrane is transparent.
```

## Core 17 (anchors)

```
proofgeneral-narya (-1)    narya-proofs (-1)       narya-hatchery (0)
lean-proof-walk (+1)       elisp (-)               emacs (-)
emacs-info (0)             cat (0)                 elements-infinity-cats (0)
topos-catcolab (0)         topos-unified (+1)      skill-creator (-)
hatchery-papers (-)        gh (-)                  code-review (-)
tree-sitter (-)            holes (0)
```

## Hop-2 Periphery (outside-in reach)

### From proofgeneral-narya → gay-mcp → ...
- `discrete-backprop`
- `langevin-dynamics`
- `splitmixternary-opine`
- `gay-fokker-planck-staging`
- `gay-monte-carlo`
- `gay-julia`
- `gay-integration`
- `gf3-constrained-animation`
- `geohash-coloring`
- `finder-color-walk`
- `chromatic-walk`
- `farben-generator`
- `glamorous-gay`

### From narya-proofs → ordered-locale → ...
- `ordered-locale-fanout`
- `ordered-locale-proper`
- `ego-locale` (GF(3)→GF(9)→GF(27) tower)
- `spectral-locale-debugging`

### From narya-proofs → sheaf-cohomology → ...
- `sheaf-laplacian-coordination`
- `sheaf-uncertainty`
- `sheaf-theoretic-coordination`
- `sheaf-cohomology-bci`
- `persistent-homology`
- `derham-cohomology`
- `coequalizers`

### From elements-infinity-cats → synthetic-adjunctions → ...
- `adjunction-algebra`
- `kan-extensions`
- `kan-extension`
- `yoneda-embedding`
- `yoneda-directed`
- `natural-transformation`
- `free-forgetful`
- `right-adjoint`
- `hom-functor`
- `universal-property`
- `galois-connections`
- `distributive-law`
- `lawvere-theory`

### From elements-infinity-cats → covariant-fibrations → ...
- `covariant-modification`
- `grothendieck-fibration`
- `categorical-composition`
- `monoidal-category`
- `end-coend`
- `virtual-double`
- `x-module-bimodule`
- `bifunctor-bridge`

### From elements-infinity-cats → segal-types → ...
- `segal-space`
- `rezk-types`
- `model-categories`
- `quillen-model`
- `infinity-operads`
- `infinity-topoi`
- `condensed-mathematics`
- `condensed-anima-qc`
- `condensed-analytic-stacks`
- `lhott-cohesive-linear`

### From topos-catcolab → acsets-relational-thinking → ...
- `acsets-algebraic-databases`
- `acsets-hatchery`
- `acsets`
- `drive-acset`
- `docs-acset`
- `tasks-acset`
- `browser-history-acset`
- `calendar-acset`
- `markov-game-acset`
- `protocol-acset`
- `lispsyntax-acset`
- `unison-acset`

### From topos-catcolab → open-games → ...
- `temporal-coalgebra`
- `free-monad-gen`
- `operad-compose`
- `operad-composition`
- `asi-polynomial-operads`
- `discopy-operads`
- `discopy`
- `discopy-functor`
- `cybernetic-open-game`
- `nashator`
- `polynomial-dynamics`
- `polynomial-social-cognition`

### From topos-catcolab → discopy → ...
- `string-diagram-rewriting-protocol`
- `recursive-string-diagrams`
- `haskell-diagrams`
- `bmorphism-diagrams`
- `zx-calculus`

### From topos-unified → world-hopping → ...
- `unworld`
- `unworlding-involution`
- `unworld-moments`
- `unworld-ombudsman`
- `unworld-color-ordinality`
- `world-runtime`
- `world-runtime-capability`
- `world-memory-worlding`
- `worlding`
- `worldmat-tidar`
- `braindance-worlds`

### From topos-unified → three-match → ...
- `three-match`
- `gf3-tripartite`
- `gf3-pr-verify`
- `gf3-neighborhood`
- `gf3-constrained-animation`
- `clj-kondo-3color`
- `crossmodal-gf3`
- `skill-validation-gf3`
- `plr-thread-coloring`
- `tripartite-decompositions`
- `triad-interleave`
- `categorical-rewriting-triad4`

### From topos-unified → bisimulation-game → ...
- `bisimulation-game`
- `three-match`
- `game-theory` (basin-hedges: 18 modules, 236 tests)
- `equilibrium`
- `saddle-node`
- `pitchfork`
- `transcritical`
- `bifurcation`
- `bifurcation-generator`
- `center-manifold`
- `hopf`
- `attractor`
- `chaotic-attractor`
- `repeller`
- `lyapunov-function`
- `lyapunov-stability`
- `stability`
- `structural-stability`
- `ergodicity`

### From emacs → org → ...
- `org-babel-execution`
- `emacs-info`
- `elisp`
- `xenodium-elisp`
- `hy-emacs`
- `geiser-chicken`
- `slime-lisp`
- `debug-buttercup`
- `proofgeneral-narya` (cycle back to core)

### From emacs → proofgeneral → ...
- `narya-hatchery` (core)
- `narya-proofs` (core)
- `holes` (core)
- `move-narya-bridge`
- `aristotle-lean`

### From gh → gh-complete → ...
- `gh-cli`
- `gh-interactome`
- `gh-complete`
- `gh-fix-ci`
- `gh-address-comments`
- `gh-skill-explorer`
- `gh-graphql-ducklake`
- `pr-creator`
- `changelog-generator`
- `github-workflow-automation`
- `github-actions-templates`
- `github-project-management`
- `github-multi-repo`
- `github-code-review`
- `github-release-management`

### From tree-sitter → AST → ...
- `entry-point-analyzer`
- `coverage-analysis`
- `code-maturity-assessor`
- `code-documentation`
- `code-refactoring`
- `code-review` (core)

### From hatchery-papers → colored operads → ...
- `narya-hatchery` (core)
- `topoi-hatchery`
- `hatchery-index`
- `radare2-hatchery`
- `acsets-hatchery`

### From cat → derivational chains → ...
- `cat-tripartite`
- `catsharp`
- `catsharp-galois`
- `catsharp-sonification`
- `cats-for-ai`
- `cats-focus-monad`
- `concatenative`
- `propagators`
- `interaction-nets`
- `lambda-calculus`
- `linear-logic`
- `hvm-runtime`
- `type-checker`

### From lean-proof-walk → ...
- `bdd-mathematical-verification`
- `chromatic-walk` (→ gay-mcp chain)
- `aristotle-lean`
- `lean4-music-topos`
- `dafny-formal-verification`
- `dafny-zig`

### Dynamical Systems Cluster (from bifurcation chain)
- `invariant-measure`
- `invariant-set`
- `periodic-orbit`
- `stable-manifold`
- `unstable-manifold`
- `limit-set`
- `semi-conjugacy`
- `birkhoff-average`
- `hyperbolicity`
- `linearization`
- `eigenvalue-stability`
- `jacobian`
- `phase-portrait-generator`
- `phase-space-transformation`
- `phase-locking`
- `vector-field`
- `trajectory`
- `initial-value-problem`
- `coupled-system`
- `synchronization`
- `kuramoto-model`
- `autopoiesis`
- `waddington-landscape`
- `koopman-generator`
- `ergodicity`
- `stochastic-resonance`
- `kolmogorov-onsager-hurst`
- `lasalle-invariance`
- `parameter-dependent`

### Security Cluster (from code-review chain)
- `semgrep`
- `semgrep-rule-creator`
- `semgrep-rule-variant-creator`
- `codeql`
- `variant-analysis`
- `static-security-analyzer`
- `constant-time-analysis`
- `constant-time-testing`
- `wycheproof`
- `audit-context-building`
- `audit-prep-assistant`
- `burp-suite`
- `burpsuite-project-parser`
- `aflpp`
- `ossfuzz`
- `libfuzzer`
- `libafl`
- `ruzzy`
- `cargo-fuzz`
- `atheris`
- `address-sanitizer`
- `insecure-defaults`
- `sandbox-escape-detector`
- `move-smith-fuzzer`
- `fuzzing-dictionary`
- `fuzzing-obstacles`

### Distributed/Web3 Cluster
- `captp`
- `goblins`
- `guile-goblins-hoot`
- `wasm-goblins`
- `shadow-goblin`
- `syrup`
- `iroh-p2p`
- `crdt`
- `time-travel-crdt`
- `derangement-crdt`
- `reversible-computing`
- `anoma-intents`
- `juvix-intents`
- `aptos-agent`
- `aptos-gf3-society`
- `aptos-trading`
- `aptos-wallet-mcp`
- `solana-vulnerability-scanner`
- `cairo-vulnerability-scanner`
- `cosmos-vulnerability-scanner`
- `substrate-vulnerability-scanner`
- `algorand-vulnerability-scanner`
- `ton-vulnerability-scanner`

### Scientific Computing Cluster
- `julia-scientific`
- `julia-gay`
- `sicmutils`
- `enzyme-autodiff`
- `active-inference-robotics`
- `affective-taxis`
- `alife`
- `true-alife`
- `curiosity-driven`
- `forward-forward-learning`
- `gflownet`
- `compression-progress`
- `kolmogorov-compression`
- `assembly-index`
- `ramanujan-expander`
- `ihara-zeta`
- `moebius-inversion`

### MCP & Agent Cluster
- `mcp-builder`
- `mcp-spec-checker`
- `mcp-tripartite`
- `mcp-orchestrator`
- `agent-o-rama`
- `asi-agent-orama`
- `asi-integrated`
- `asi-polynomial-operads`
- `skill-dispatch`
- `skill-loader`
- `skill-creator` (core)
- `skill-installer`
- `skill-evolution`
- `skill-bonds`
- `skill-connectivity-hub`
- `skill-embedding-vss`
- `skill-validation-gf3`
- `triadic-skill-orchestrator`
- `triadic-skill-loader`
- `parallel-fanout`
- `spi-parallel-verify`

## How This Works

Loading this skill puts you within **one hop** of the entire ~600 skill graph:

1. **Direct reach**: All hop-2 periphery skills listed above (~300)
2. **Direct reach**: All 17 core hubs
3. **Implied reach**: Every hop-1 skill (~95-250) sits between a listed hop-2 node and a listed core hub

The hop-1 layer doesn't need explicit listing because it's the **intersection** of neighborhoods. Any skill that was previously at distance 2 is now at distance 1 through this membrane.

```
Before:  core ──hop1──hop2  (distance 2)
After:   core ──THIS──hop2  (distance 1)
                 │
                hop1 ← naturally traversed
```

## GF(3) Conservation

This skill is ERGODIC (0) — it doesn't generate or validate, it **mediates**.

```
proofreader-interleave (0) ⊗ proofgeneral-narya (-1) ⊗ lean-proof-walk (+1) = 0 ✓
proofreader-interleave (0) ⊗ narya-proofs (-1) ⊗ topos-unified (+1) = 0 ✓
proofreader-interleave (0) ⊗ security (-1) ⊗ agent-o-rama (+1) = 0 ✓
```

## Invocation

This skill is not invoked directly — it's a **topology skill**.
Loading it reshapes the reachability graph of the current session.

When another skill needs to find a path to a distant skill,
this interleave provides the shortcut through the membrane.
