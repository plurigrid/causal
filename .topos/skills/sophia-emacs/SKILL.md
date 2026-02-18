---
name: sophia-emacs
description: sophia-labs/mnemosyne-mcp in Emacs, configured for Claude Code, Codex, Droid, Gemini, Copilot, Vibe, and Kimi CLIs. RDF knowledge graphs as proof/session memory across all local AI agents.
version: 0.1.0
---

# Sophia Emacs — Mnemosyne MCP for All Local AI CLIs

**Trit**: +1 (PLUS — generates knowledge graph state)
**Source**: [sophia-labs/mnemosyne-mcp](https://github.com/sophia-labs/mnemosyne-mcp)

## Overview

This skill configures `neem` (mnemosyne-mcp) as a shared knowledge layer
accessible from Emacs AND all local AI CLI tools simultaneously.
Every agent writes to the same RDF graph. Every agent reads the same wires.

## Install neem

```bash
# Clone and install
git clone https://github.com/sophia-labs/mnemosyne-mcp.git ~/i/mnemosyne-mcp
cd ~/i/mnemosyne-mcp
uv tool install -e .

# Initialize (OAuth or dev token)
neem init

# Or use dev mode (no OAuth)
export MNEMOSYNE_DEV_TOKEN="dev-local"
export MNEMOSYNE_DEV_USER_ID="bmorphism"
export MNEMOSYNE_FASTAPI_URL="http://127.0.0.1:8001"
```

## CLI Configurations

### 1. Claude Code

```bash
# Register MCP server
claude mcp add mnemosyne -- uv run neem-mcp-server

# Or in ~/.claude.json
```

```json
{
  "mcpServers": {
    "mnemosyne": {
      "command": "uv",
      "args": ["run", "neem-mcp-server"],
      "env": {
        "MNEMOSYNE_FASTAPI_URL": "http://127.0.0.1:8001",
        "MNEMOSYNE_DEV_TOKEN": "dev-local",
        "MNEMOSYNE_DEV_USER_ID": "bmorphism"
      }
    }
  }
}
```

### 2. OpenAI Codex CLI

```bash
# Codex uses MCP via config
# ~/.codex/config.json
```

```json
{
  "mcpServers": {
    "mnemosyne": {
      "command": "uv",
      "args": ["run", "neem-mcp-server"],
      "env": {
        "MNEMOSYNE_FASTAPI_URL": "http://127.0.0.1:8001",
        "LOG_LEVEL": "ERROR"
      }
    }
  }
}
```

### 3. Google Gemini CLI

```bash
# Gemini CLI MCP config
# ~/.gemini/settings.json
```

```json
{
  "mcpServers": {
    "mnemosyne": {
      "command": "uv",
      "args": ["run", "neem-mcp-server"],
      "env": {
        "MNEMOSYNE_FASTAPI_URL": "http://127.0.0.1:8001"
      }
    }
  }
}
```

### 4. GitHub Copilot CLI

```bash
# Copilot uses VS Code MCP settings or CLI config
# ~/.config/github-copilot/mcp.json
```

```json
{
  "servers": {
    "mnemosyne": {
      "command": "uv",
      "args": ["run", "neem-mcp-server"],
      "env": {
        "MNEMOSYNE_FASTAPI_URL": "http://127.0.0.1:8001"
      }
    }
  }
}
```

### 5. Droid (Android Studio AI)

```bash
# Droid/Android Studio MCP
# Project-level .droid/mcp.json or user-level
```

```json
{
  "mcpServers": {
    "mnemosyne": {
      "command": "uv",
      "args": ["run", "neem-mcp-server"],
      "env": {
        "MNEMOSYNE_FASTAPI_URL": "http://127.0.0.1:8001"
      }
    }
  }
}
```

### 6. Vibe (macOS → DGX offload)

```bash
# Vibe uses babashka for MCP orchestration
# ~/i/vibe/mcp-config.edn
```

```clojure
{:mcp-servers
 {:mnemosyne
  {:command "uv"
   :args ["run" "neem-mcp-server"]
   :env {"MNEMOSYNE_FASTAPI_URL" "http://127.0.0.1:8001"
         "MNEMOSYNE_DEV_TOKEN" "dev-local"}}}}
```

### 7. Kimi CLI (Moonshot)

```bash
# Kimi k1 CLI MCP configuration
# ~/.kimi/mcp.json
```

```json
{
  "mcpServers": {
    "mnemosyne": {
      "command": "uv",
      "args": ["run", "neem-mcp-server"],
      "env": {
        "MNEMOSYNE_FASTAPI_URL": "http://127.0.0.1:8001"
      }
    }
  }
}
```

## Emacs Integration

```elisp
;;; sophia-mnemosyne.el --- Mnemosyne MCP from Emacs -*- lexical-binding: t -*-

(defgroup sophia-mnemosyne nil
  "Mnemosyne knowledge graph integration."
  :group 'tools
  :prefix "sophia-mnemosyne-")

(defcustom sophia-mnemosyne-url "http://127.0.0.1:8001"
  "Mnemosyne FastAPI backend URL."
  :type 'string
  :group 'sophia-mnemosyne)

(defun sophia-mnemosyne--api (method endpoint &optional body)
  "Call Mnemosyne API with METHOD on ENDPOINT, optional BODY."
  (let* ((url-request-method method)
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data (when body (json-encode body)))
         (url (concat sophia-mnemosyne-url endpoint)))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun sophia-mnemosyne-list-graphs ()
  "List all knowledge graphs."
  (interactive)
  (let ((graphs (sophia-mnemosyne--api "GET" "/graphs")))
    (with-current-buffer (get-buffer-create "*Mnemosyne Graphs*")
      (erase-buffer)
      (insert (format "Mnemosyne Knowledge Graphs\n%s\n\n"
                      (make-string 40 ?─)))
      (mapc (lambda (g)
              (insert (format "  %s\n" (alist-get 'name g))))
            graphs)
      (display-buffer (current-buffer)))))

(defun sophia-mnemosyne-sparql-query (query)
  "Execute SPARQL QUERY against current graph."
  (interactive "sSPARQL: ")
  (let ((result (sophia-mnemosyne--api
                 "POST" "/sparql"
                 `((query . ,query)))))
    (with-current-buffer (get-buffer-create "*SPARQL Results*")
      (erase-buffer)
      (insert (pp-to-string result))
      (display-buffer (current-buffer)))))

(defun sophia-mnemosyne-write-proof-transcript (name content)
  "Write proof transcript NAME with CONTENT to current graph."
  (interactive "sDocument name: \nsContent: ")
  (sophia-mnemosyne--api
   "POST" "/documents"
   `((name . ,name) (content . ,content)))
  (message "Proof transcript '%s' saved to Mnemosyne." name))

(defun sophia-mnemosyne-save-buffer-as-transcript ()
  "Save current buffer contents as a Mnemosyne proof transcript."
  (interactive)
  (let ((name (file-name-nondirectory (or (buffer-file-name) (buffer-name))))
        (content (buffer-string)))
    (sophia-mnemosyne-write-proof-transcript name content)))

(defun sophia-mnemosyne-create-wire (from-id to-id predicate)
  "Create semantic wire from FROM-ID to TO-ID with PREDICATE."
  (interactive "sFrom ID: \nsTo ID: \nsPredicate: ")
  (sophia-mnemosyne--api
   "POST" "/wires"
   `((from . ,from-id) (to . ,to-id) (predicate . ,predicate)))
  (message "Wire created: %s -[%s]-> %s" from-id predicate to-id))
```

## Unified Setup Script

```bash
#!/bin/bash
# setup-mnemosyne-all-clis.sh
# Run once to configure mnemosyne-mcp for all local AI CLIs

set -e

NEEM_CMD="uv run neem-mcp-server"
API_URL="http://127.0.0.1:8001"

echo "=== Setting up mnemosyne-mcp for all AI CLIs ==="

# 1. Claude Code
if command -v claude &>/dev/null; then
  claude mcp add mnemosyne -- $NEEM_CMD
  echo "✓ Claude Code configured"
fi

# 2. Codex
mkdir -p ~/.codex
cat > ~/.codex/mcp.json << 'EOF'
{"mcpServers":{"mnemosyne":{"command":"uv","args":["run","neem-mcp-server"],"env":{"MNEMOSYNE_FASTAPI_URL":"http://127.0.0.1:8001","LOG_LEVEL":"ERROR"}}}}
EOF
echo "✓ Codex configured"

# 3. Gemini
mkdir -p ~/.gemini
cat > ~/.gemini/settings.json << 'EOF'
{"mcpServers":{"mnemosyne":{"command":"uv","args":["run","neem-mcp-server"],"env":{"MNEMOSYNE_FASTAPI_URL":"http://127.0.0.1:8001"}}}}
EOF
echo "✓ Gemini CLI configured"

# 4. Copilot
mkdir -p ~/.config/github-copilot
cat > ~/.config/github-copilot/mcp.json << 'EOF'
{"servers":{"mnemosyne":{"command":"uv","args":["run","neem-mcp-server"],"env":{"MNEMOSYNE_FASTAPI_URL":"http://127.0.0.1:8001"}}}}
EOF
echo "✓ Copilot configured"

# 5. Kimi
mkdir -p ~/.kimi
cat > ~/.kimi/mcp.json << 'EOF'
{"mcpServers":{"mnemosyne":{"command":"uv","args":["run","neem-mcp-server"],"env":{"MNEMOSYNE_FASTAPI_URL":"http://127.0.0.1:8001"}}}}
EOF
echo "✓ Kimi configured"

echo ""
echo "=== All CLIs configured ==="
echo "Start the backend: kubectl port-forward svc/mnemosyne-api 8080:80"
echo "Or dev mode: MNEMOSYNE_FASTAPI_URL=$API_URL neem serve"
```

## Proof Session Workflow

```
1. Start proof in Emacs (Narya/Lean/Coq)
2. Use C-o (causal-proofreader-tmenu) for navigation
3. M-x sophia-mnemosyne-save-buffer-as-transcript  → saves to RDF graph
4. M-x sophia-mnemosyne-create-wire                → link proof dependencies
5. Any CLI (claude/codex/gemini/copilot/kimi) can query the same graph:
   - "What theorems depend on lemma X?"
   - "Show me the tactic sequence for proof Y"
   - "Find all proofs using induction on Nat"
```

## GF(3) Triad

```
sophia-emacs (+1) ⊗ causal-proofreader (0) ⊗ narya-proofs (-1) = 0 ✓
```

## Related Skills

- `causal-proofreader` (0): Transient menus for proof assistants
- `proofreader-interleave` (0): Outside-in membrane skill
- `narya-proofs` (-1): Proof verification certificates
- `captp` (-1): OCapN transport layer
