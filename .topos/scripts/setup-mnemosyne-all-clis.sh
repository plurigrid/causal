#!/bin/bash
# setup-mnemosyne-all-clis.sh
# Configure mnemosyne-mcp for all local AI CLIs
# Source: sophia-labs/mnemosyne-mcp

set -e

API_URL="${MNEMOSYNE_FASTAPI_URL:-http://127.0.0.1:8001}"
DEV_TOKEN="${MNEMOSYNE_DEV_TOKEN:-dev-local}"
DEV_USER="${MNEMOSYNE_DEV_USER_ID:-bmorphism}"

MCP_JSON=$(cat <<ENDJSON
{"mcpServers":{"mnemosyne":{"command":"uv","args":["run","neem-mcp-server"],"env":{"MNEMOSYNE_FASTAPI_URL":"${API_URL}","MNEMOSYNE_DEV_TOKEN":"${DEV_TOKEN}","MNEMOSYNE_DEV_USER_ID":"${DEV_USER}","LOG_LEVEL":"ERROR"}}}}
ENDJSON
)

echo "=== Mnemosyne MCP — Multi-CLI Setup ==="
echo "API: ${API_URL}"
echo ""

# 0. Install neem if not present
if ! command -v neem &>/dev/null; then
  echo "Installing neem..."
  if [ -d "$HOME/i/mnemosyne-mcp" ]; then
    cd "$HOME/i/mnemosyne-mcp"
    uv tool install -e .
  else
    echo "  Clone first: git clone https://github.com/sophia-labs/mnemosyne-mcp.git ~/i/mnemosyne-mcp"
    echo "  Then re-run this script."
  fi
fi

# 1. Claude Code
if command -v claude &>/dev/null; then
  claude mcp add mnemosyne -- uv run neem-mcp-server 2>/dev/null && echo "✓ Claude Code" || echo "⚠ Claude Code (may already exist)"
else
  echo "○ Claude Code CLI not found, skipping"
fi

# 2. Codex (OpenAI)
if command -v codex &>/dev/null; then
  mkdir -p ~/.codex
  echo "${MCP_JSON}" > ~/.codex/mcp.json
  echo "✓ Codex"
else
  echo "○ Codex CLI not found, writing config anyway"
  mkdir -p ~/.codex
  echo "${MCP_JSON}" > ~/.codex/mcp.json
fi

# 3. Gemini CLI
mkdir -p ~/.gemini
echo "${MCP_JSON}" > ~/.gemini/settings.json
echo "✓ Gemini CLI"

# 4. GitHub Copilot
mkdir -p ~/.config/github-copilot
# Copilot uses different key name
COPILOT_JSON=$(cat <<ENDJSON2
{"servers":{"mnemosyne":{"command":"uv","args":["run","neem-mcp-server"],"env":{"MNEMOSYNE_FASTAPI_URL":"${API_URL}","LOG_LEVEL":"ERROR"}}}}
ENDJSON2
)
echo "${COPILOT_JSON}" > ~/.config/github-copilot/mcp.json
echo "✓ Copilot"

# 5. Droid
mkdir -p ~/.droid
echo "${MCP_JSON}" > ~/.droid/mcp.json
echo "✓ Droid"

# 6. Vibe (babashka config)
mkdir -p ~/i/vibe
cat > ~/i/vibe/mcp-mnemosyne.edn <<EOF
{:mcp-servers
 {:mnemosyne
  {:command "uv"
   :args ["run" "neem-mcp-server"]
   :env {"MNEMOSYNE_FASTAPI_URL" "${API_URL}"
         "MNEMOSYNE_DEV_TOKEN" "${DEV_TOKEN}"
         "MNEMOSYNE_DEV_USER_ID" "${DEV_USER}"}}}}
EOF
echo "✓ Vibe"

# 7. Kimi CLI
mkdir -p ~/.kimi
echo "${MCP_JSON}" > ~/.kimi/mcp.json
echo "✓ Kimi"

echo ""
echo "=== Done. ${API_URL} configured for 7 CLIs ==="
echo ""
echo "Next steps:"
echo "  1. Start backend: neem serve (or kubectl port-forward)"
echo "  2. Test: neem init"
echo "  3. Use from any CLI: list_graphs, sparql_query, write_document"
