;;; causal-self-walker-test-utils.el --- Test utilities for self-walker -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools

;;; Commentary:

;; Shared fixtures and helpers for causal-self-walker ERT tests.

;;; Code:
(require 'ert)

(defconst causal-self-walker-test--sample-goals-text
  "h1 : Nat
h2 : h1 > 0
⊢ h1 + 1 > 1"
  "Sample *Goals* buffer text for testing.")

(defconst causal-self-walker-test--no-goals-texts
  '("No Goals" "No more goals" "QED" "No goals")
  "Strings that indicate proof completion.")

(defconst causal-self-walker-test--source-with-tactics
  "(causal-proof--insert-tactic \"intro\")
(causal-proof--insert-tactic \"apply\")
(causal-proof--insert-tactic \"exact\")
(causal-proofreader--insert-tactic \"rewrite\")
(causal-proofreader--insert-tactic \"simp\")
(causal-proofreader--insert-tactic \"intro\")"
  "Mock Elisp source containing tactic insertions with a duplicate.")

(provide 'causal-self-walker-test-utils)
;;; causal-self-walker-test-utils.el ends here
