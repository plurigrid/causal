# .topos — Causal ProofReader Knowledge Layer

Transcripts, skills, and MCP integrations for `causal-proofreader`.

## Structure

```
.topos/
├── README.md
├── mcp/
│   └── mnemosyne.json           # sophia-labs/mnemosyne-mcp config
├── transcripts/
│   └── 2026-02-18-init.md
├── scripts/
│   └── setup-mnemosyne-all-clis.bb  # Configure neem for 7 AI CLIs
├── skills/
│   ├── casual-proofreader/      # Transient menus for proof assistants
│   │   └── SKILL.md
│   ├── sophia-emacs/            # Mnemosyne RDF from Emacs + all CLIs
│   │   └── SKILL.md
│   ├── catcolab/                # CatColab categorical modeling
│   │   └── SKILL.md
│   └── proofreader-interleave/  # Outside-in membrane skill
│       └── SKILL.md
└── references/
```

## Elisp Packages

| File | Description |
|---|---|
| `lisp/causal-proofreader.el` | Transient UI for Proof General / Lean 4 / Narya |
| `lisp/causal-proofreader-utils.el` | Backend detection, Unicode DB, dispatch |
| `lisp/causal-proofreader-settings.el` | Settings menu |
| `lisp/sophia-mnemosyne.el` | Emacs client for Mnemosyne RDF knowledge graph |
| `lisp/causal-catcolab.el` | Transient UI for CatColab (ologs, causal loops, Petri nets) |
| `lisp/causal-catcolab-utils.el` | JSON-RPC 2.0 client for CatColab backend |

## MCP Integration: Mnemosyne

[sophia-labs/mnemosyne-mcp](https://github.com/sophia-labs/mnemosyne-mcp) (`neem`)
provides RDF knowledge graph storage for proof artifacts:

- **Graphs** → proof project namespaces
- **Documents/Blocks** → proof states, tactic sequences
- **Wires** → semantic connections between proof steps (bridge types)
- **SPARQL** → query proof relationships

Configured for 7 CLIs: Claude Code, Codex, Gemini, Copilot, Droid, Vibe, Kimi.

## GF(3) Triads

```
causal-proofreader (0) ⊗ catcolab (-1) ⊗ sophia-emacs (+1) = 0
sophia-emacs (+1) ⊗ causal-proofreader (0) ⊗ narya-proofs (-1) = 0
```
