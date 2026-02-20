;;; test-causal-self-walker-paperproof.el --- ERT tests for paperproof integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools

;;; Commentary:

;; Offline ERT tests for causal-self-walker-paperproof.el.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'causal-self-walker)
(unless (featurep 'causal-self-walker-paperproof)
  (require 'causal-self-walker-paperproof))

;;; ── Helpers ──────────────────────────────────────────────────────────

(defun test-pp--make-state (step hyps goal tactic source status)
  "Create a test state struct."
  (causal-self-walker-state-create
   :step step :hypotheses hyps :goal goal
   :tactic tactic :source-fn source :status status))

(defvar test-pp--sample-chain
  (list
   (test-pp--make-state 2 '(("x" . "Nat")) "No Goals" "rfl" "step-forward" 'qed)
   (test-pp--make-state 1 '(("x" . "Nat")) "x = x" "intro x" "step-forward" 'active)
   (test-pp--make-state 0 nil "forall x, x = x" "" "" 'initial))
  "Sample reversed chain (most recent first, as stored internally).")

;;; ── Feature loading ──────────────────────────────────────────────────

(ert-deftest test-paperproof-loads ()
  "causal-self-walker-paperproof feature is available."
  (should (featurep 'causal-self-walker-paperproof)))

;;; ── Node construction ────────────────────────────────────────────────

(ert-deftest test-paperproof-hyp-nodes-basic ()
  "Hypothesis nodes have correct structure."
  (let ((nodes (causal-self-walker-paperproof--hyp-nodes '(("x" . "Nat")) 1)))
    (should (= 1 (length nodes)))
    (let ((n (car nodes)))
      (should (string= "hypothesis" (alist-get 'type n)))
      (should (string= "x" (alist-get 'name n)))
      (should (string= "Nat" (alist-get 'value n)))
      (should (= 1 (alist-get 'step n))))))

(ert-deftest test-paperproof-hyp-nodes-multiple ()
  "Multiple hypotheses produce multiple nodes."
  (let ((nodes (causal-self-walker-paperproof--hyp-nodes
                '(("a" . "Type") ("b" . "a -> a")) 3)))
    (should (= 2 (length nodes)))
    (should (string= "a" (alist-get 'name (car nodes))))
    (should (string= "b" (alist-get 'name (cadr nodes))))))

(ert-deftest test-paperproof-hyp-nodes-empty ()
  "Empty hypotheses produce empty node list."
  (should (null (causal-self-walker-paperproof--hyp-nodes nil 0))))

(ert-deftest test-paperproof-goal-node ()
  "Goal node has correct structure."
  (let ((n (causal-self-walker-paperproof--goal-node "x = x" 2)))
    (should (string= "goal" (alist-get 'type n)))
    (should (string= "x = x" (alist-get 'value n)))
    (should (= 2 (alist-get 'step n)))
    (should (string= causal-self-walker-paperproof-color-goals
                     (alist-get 'color n)))))

(ert-deftest test-paperproof-tactic-node ()
  "Tactic node has correct structure."
  (let ((n (causal-self-walker-paperproof--tactic-node "intro x" "step-forward" 1)))
    (should (string= "tactic" (alist-get 'type n)))
    (should (string= "intro x" (alist-get 'name n)))
    (should (string= "step-forward" (alist-get 'source n)))
    (should (= 1 (alist-get 'step n)))))

;;; ── Box conversion ───────────────────────────────────────────────────

(ert-deftest test-paperproof-state-to-box ()
  "State is correctly converted to a box structure."
  (let* ((state (test-pp--make-state
                 1 '(("h" . "P")) "Q" "apply f" "step-forward" 'active))
         (box (causal-self-walker-paperproof--state-to-box state)))
    (should (string= "step-1" (alist-get 'id box)))
    (should (= 1 (alist-get 'step box)))
    (should (string= "active" (alist-get 'status box)))
    (should (= 1 (length (alist-get 'hypotheses box))))
    (should (alist-get 'goal box))
    (should (alist-get 'tactic box))))

(ert-deftest test-paperproof-state-to-box-no-tactic ()
  "Initial state with empty tactic produces nil tactic field."
  (let* ((state (test-pp--make-state 0 nil "P -> Q" "" "" 'initial))
         (box (causal-self-walker-paperproof--state-to-box state)))
    (should (null (alist-get 'tactic box)))))

;;; ── Chain to JSON ────────────────────────────────────────────────────

(ert-deftest test-paperproof-chain-to-json-structure ()
  "Chain-to-JSON produces valid top-level structure."
  (let ((causal-self-walker--chain test-pp--sample-chain))
    (let ((json (causal-self-walker-paperproof-chain-to-json
                 test-pp--sample-chain "test proof")))
      (should (string= "1.0.0" (alist-get 'version json)))
      (should (string= "test proof" (alist-get 'name json)))
      (should (= 3 (alist-get 'steps json)))
      (should (vectorp (alist-get 'boxes json)))
      (should (vectorp (alist-get 'edges json))))))

(ert-deftest test-paperproof-chain-to-json-completed ()
  "Chain ending in QED is marked completed."
  (let ((causal-self-walker--chain test-pp--sample-chain))
    (let ((json (causal-self-walker-paperproof-chain-to-json
                 test-pp--sample-chain)))
      (should (eq t (alist-get 'completed json))))))

(ert-deftest test-paperproof-chain-to-json-incomplete ()
  "Chain not ending in QED is marked incomplete."
  (let* ((chain (list (test-pp--make-state 1 nil "P" "intro" "sf" 'active)
                       (test-pp--make-state 0 nil "P -> Q" "" "" 'initial)))
         (causal-self-walker--chain chain))
    (let ((json (causal-self-walker-paperproof-chain-to-json chain)))
      (should (eq :json-false (alist-get 'completed json))))))

;;; ── Edges ────────────────────────────────────────────────────────────

(ert-deftest test-paperproof-edges-count ()
  "N states produce N-1 edges."
  (let* ((chain (reverse test-pp--sample-chain))
         (edges (causal-self-walker-paperproof--edges chain)))
    (should (= 2 (length edges)))))

(ert-deftest test-paperproof-edges-single-state ()
  "Single state produces no edges."
  (let* ((chain (list (test-pp--make-state 0 nil "P" "" "" 'initial)))
         (edges (causal-self-walker-paperproof--edges chain)))
    (should (= 0 (length edges)))))

(ert-deftest test-paperproof-edges-structure ()
  "Edge has from, to, and tactic fields."
  (let* ((chain (list (test-pp--make-state 0 nil "P" "" "" 'initial)
                       (test-pp--make-state 1 nil "Q" "apply" "sf" 'active)))
         (edges (causal-self-walker-paperproof--edges chain)))
    (let ((e (car edges)))
      (should (string= "step-0" (alist-get 'from e)))
      (should (string= "step-1" (alist-get 'to e)))
      (should (string= "apply" (alist-get 'tactic e))))))

;;; ── ASCII rendering ──────────────────────────────────────────────────

(ert-deftest test-paperproof-render-box-contains-step ()
  "Rendered box contains step number."
  (let* ((state (test-pp--make-state 3 '(("x" . "Nat")) "x = 0" "simp" "sf" 'active))
         (box (causal-self-walker-paperproof--state-to-box state))
         (rendered (causal-self-walker-paperproof--render-box box)))
    (should (string-match-p "Step 3" rendered))))

(ert-deftest test-paperproof-render-box-contains-hyps ()
  "Rendered box contains hypothesis text."
  (let* ((state (test-pp--make-state 1 '(("h" . "P")) "Q" "apply" "sf" 'active))
         (box (causal-self-walker-paperproof--state-to-box state))
         (rendered (causal-self-walker-paperproof--render-box box)))
    (should (string-match-p "h : P" rendered))
    (should (string-match-p "Hypotheses" rendered))))

(ert-deftest test-paperproof-render-box-contains-goal ()
  "Rendered box contains goal text."
  (let* ((state (test-pp--make-state 0 nil "forall x, x = x" "" "" 'initial))
         (box (causal-self-walker-paperproof--state-to-box state))
         (rendered (causal-self-walker-paperproof--render-box box)))
    (should (string-match-p "forall x, x = x" rendered))
    (should (string-match-p "Goal" rendered))))

(ert-deftest test-paperproof-render-box-contains-tactic ()
  "Rendered box contains tactic when present."
  (let* ((state (test-pp--make-state 2 nil "True" "trivial" "sf" 'active))
         (box (causal-self-walker-paperproof--state-to-box state))
         (rendered (causal-self-walker-paperproof--render-box box)))
    (should (string-match-p "Tactic" rendered))
    (should (string-match-p "trivial" rendered))))

(ert-deftest test-paperproof-render-full ()
  "Full render includes header and multiple boxes."
  (let* ((causal-self-walker--chain test-pp--sample-chain)
         (json (causal-self-walker-paperproof-chain-to-json
                test-pp--sample-chain "my proof"))
         (rendered (causal-self-walker-paperproof-render json)))
    (should (string-match-p "Paperproof: my proof" rendered))
    (should (string-match-p "QED" rendered))
    (should (string-match-p "Step 0" rendered))
    (should (string-match-p "Step 1" rendered))
    (should (string-match-p "Step 2" rendered))))

;;; ── HTML generation ──────────────────────────────────────────────────

(ert-deftest test-paperproof-html-escape ()
  "HTML special chars are escaped."
  (should (string= "&amp;&lt;&gt;&quot;"
                    (causal-self-walker-paperproof--html-escape "&<>\""))))

(ert-deftest test-paperproof-html-escape-nil ()
  "Nil input produces empty string."
  (should (string= "" (causal-self-walker-paperproof--html-escape nil))))

(ert-deftest test-paperproof-generate-html-structure ()
  "Generated HTML has required structure."
  (let* ((causal-self-walker--chain test-pp--sample-chain)
         (json (causal-self-walker-paperproof-chain-to-json
                test-pp--sample-chain "test"))
         (html (causal-self-walker-paperproof--generate-html json)))
    (should (string-match-p "<!DOCTYPE html>" html))
    (should (string-match-p "<title>Paperproof: test</title>" html))
    (should (string-match-p "proof-tree" html))
    (should (string-match-p "window\\.paperproofData" html))))

(ert-deftest test-paperproof-html-box ()
  "HTML box contains key elements."
  (let* ((state (test-pp--make-state 1 '(("h" . "P")) "Q" "apply" "sf" 'active))
         (box (causal-self-walker-paperproof--state-to-box state))
         (html (causal-self-walker-paperproof--html-box box)))
    (should (string-match-p "Step 1" html))
    (should (string-match-p "ACTIVE" html))
    (should (string-match-p "h : P" html))
    (should (string-match-p "apply" html))))

;;; ── Chain nodes utility ──────────────────────────────────────────────

(ert-deftest test-paperproof-chain-nodes-count ()
  "Chain nodes returns all hypothesis, goal, and tactic nodes."
  (let* ((causal-self-walker--chain test-pp--sample-chain)
         (nodes (causal-self-walker-paperproof-chain-nodes
                 test-pp--sample-chain)))
    ;; State 0: 0 hyps + 1 goal + 0 tactics = 1
    ;; State 1: 1 hyp + 1 goal + 1 tactic = 3
    ;; State 2: 1 hyp + 1 goal + 1 tactic = 3
    ;; Total = 7
    (should (= 7 (length nodes)))))

(ert-deftest test-paperproof-chain-nodes-types ()
  "Chain nodes include all three types."
  (let* ((causal-self-walker--chain test-pp--sample-chain)
         (nodes (causal-self-walker-paperproof-chain-nodes
                 test-pp--sample-chain))
         (types (mapcar (lambda (n) (alist-get 'type n)) nodes)))
    (should (member "hypothesis" types))
    (should (member "goal" types))
    (should (member "tactic" types))))

;;; ── JSON export (file I/O) ───────────────────────────────────────────

(ert-deftest test-paperproof-export-json-roundtrip ()
  "JSON export can be parsed back."
  (let* ((causal-self-walker--chain test-pp--sample-chain)
         (json (causal-self-walker-paperproof-chain-to-json
                test-pp--sample-chain "roundtrip"))
         (encoded (json-encode json))
         (decoded (json-read-from-string encoded)))
    (should (string= "roundtrip" (alist-get 'name decoded)))
    (should (= 3 (alist-get 'steps decoded)))))

;;; ── Customization defaults ───────────────────────────────────────────

(ert-deftest test-paperproof-default-colors ()
  "Default color values are set."
  (should (stringp causal-self-walker-paperproof-color-hypotheses))
  (should (stringp causal-self-walker-paperproof-color-goals))
  (should (stringp causal-self-walker-paperproof-color-tactics))
  (should (string-prefix-p "#" causal-self-walker-paperproof-color-hypotheses))
  (should (string-prefix-p "#" causal-self-walker-paperproof-color-goals)))

(ert-deftest test-paperproof-default-export-dir ()
  "Default export directory is set."
  (should (stringp causal-self-walker-paperproof-export-dir)))

(provide 'test-causal-self-walker-paperproof)
;;; test-causal-self-walker-paperproof.el ends here
