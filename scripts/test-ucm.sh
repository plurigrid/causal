#!/usr/bin/env bash
# Run causal-ucm ERT tests standalone (no ELPA dependencies needed).
# Usage: ./scripts/test-ucm.sh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"

echo "=== causal-ucm core tests ==="
emacs --batch \
  -L "$REPO_DIR/lisp" \
  -L "$REPO_DIR/tests" \
  --eval "(require 'test-causal-ucm)" \
  -f ert-run-tests-batch-and-exit

echo ""
echo "=== causal-ucm-scratch tests ==="
emacs --batch \
  -L "$REPO_DIR/lisp" \
  -L "$REPO_DIR/tests" \
  --eval "(require 'test-causal-ucm-scratch)" \
  -f ert-run-tests-batch-and-exit
