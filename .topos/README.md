# .topos — Causal ProofReader Knowledge Layer

Transcripts, skills, and MCP integrations for `casual-proofreader`.

## Structure

```
.topos/
├── README.md              # This file
├── mcp/                   # MCP server configs
│   └── mnemosyne.json     # sophia-labs/mnemosyne-mcp integration
├── transcripts/           # Proof session transcripts
├── skills/                # Local skill definitions
│   └── casual-proofreader/
└── references/            # Papers, notes
```

## MCP Integration: Mnemosyne

[sophia-labs/mnemosyne-mcp](https://github.com/sophia-labs/mnemosyne-mcp) (`neem`)
provides RDF knowledge graph storage for proof artifacts:

- **Graphs** → proof project namespaces
- **Documents/Blocks** → proof states, tactic sequences
- **Wires** → semantic connections between proof steps (bridge types)
- **SPARQL** → query proof relationships

## GF(3) Triad

```
casual-proofreader (0) ⊗ proofgeneral-narya (-1) ⊗ lean-proof-walk (+1) = 0
```
