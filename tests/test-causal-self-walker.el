;;; test-causal-self-walker.el --- ERT tests for causal-self-walker -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools

;;; Commentary:

;; Offline ERT tests for causal-self-walker.el.
;; No live proof assistant sessions required.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'causal-self-walker-test-utils)
(require 'causal-self-walker)

;;; ── Feature loading ──────────────────────────────────────────────────────

(ert-deftest test-self-walker-loads ()
  "causal-self-walker feature is available."
  (should (featurep 'causal-self-walker)))

;;; ── Tactic introspection ─────────────────────────────────────────────────

(ert-deftest test-self-walker-discover-from-file ()
  "Discover tactics from a mock Elisp source file."
  (let ((tmpfile (make-temp-file "self-walker-test" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert causal-self-walker-test--source-with-tactics))
          (let ((result (causal-self-walker--discover-tactics-from-file tmpfile)))
            (should (listp result))
            (should (member "intro" result))
            (should (member "apply" result))
            (should (member "exact" result))
            (should (member "rewrite" result))
            (should (member "simp" result))))
      (delete-file tmpfile))))

(ert-deftest test-self-walker-discover-deduplicates ()
  "Discovered tactics are deduplicated."
  (let ((tmpfile (make-temp-file "self-walker-test" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert causal-self-walker-test--source-with-tactics))
          (let ((result (causal-self-walker--discover-tactics-from-file tmpfile)))
            ;; "intro" appears twice in the source, should only appear once
            (should (= 1 (cl-count "intro" result :test #'string=)))))
      (delete-file tmpfile))))

(ert-deftest test-self-walker-discover-nil-on-missing-file ()
  "Returns nil for a non-existent file."
  (should (null (causal-self-walker--discover-tactics-from-file
                 "/nonexistent/file.el"))))

(ert-deftest test-self-walker-discover-nil-on-nil ()
  "Returns nil when file argument is nil."
  (should (null (causal-self-walker--discover-tactics-from-file nil))))

;;; ── State struct ─────────────────────────────────────────────────────────

(ert-deftest test-self-walker-state-create ()
  "causal-self-walker-state-create builds a valid struct."
  (let ((s (causal-self-walker-state-create
            :step 3
            :hypotheses '(("h" . "Nat"))
            :goal "h = 0"
            :tactic "intro"
            :source-fn "causal-proof-step-forward"
            :status 'active)))
    (should (= 3 (causal-self-walker-state-step s)))
    (should (equal '(("h" . "Nat")) (causal-self-walker-state-hypotheses s)))
    (should (string= "h = 0" (causal-self-walker-state-goal s)))
    (should (string= "intro" (causal-self-walker-state-tactic s)))
    (should (eq 'active (causal-self-walker-state-status s)))))

;;; ── No-goals detection ──────────────────────────────────────────────────

(ert-deftest test-self-walker-no-goals-nil ()
  "Nil goal is detected as no-goals."
  (should (causal-self-walker--no-goals-p nil)))

(ert-deftest test-self-walker-no-goals-empty ()
  "Empty string goal is detected as no-goals."
  (should (causal-self-walker--no-goals-p "")))

(ert-deftest test-self-walker-no-goals-strings ()
  "Known no-goals strings are detected."
  (dolist (text causal-self-walker-test--no-goals-texts)
    (should (causal-self-walker--no-goals-p text))))

(ert-deftest test-self-walker-no-goals-active ()
  "An active goal string is NOT detected as no-goals."
  (should-not (causal-self-walker--no-goals-p "h1 + 1 > 1")))

;;; ── Chain recording ──────────────────────────────────────────────────────

(ert-deftest test-self-walker-record-state ()
  "Recording a state pushes it onto the chain."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 42))
    (causal-self-walker--record-state 0 '(("h" . "Nat")) "h = 0" "" "" 'initial)
    (should (= 1 (length causal-self-walker--chain)))
    (should (= 0 (causal-self-walker-state-step (car causal-self-walker--chain))))))

(ert-deftest test-self-walker-chain-ordered ()
  "Chain-ordered returns states in chronological order."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 0))
    (causal-self-walker--record-state 0 nil "goal0" "" "" 'initial)
    (causal-self-walker--record-state 1 nil "goal1" "intro" "fn" 'active)
    (causal-self-walker--record-state 2 nil "" "" "" 'qed)
    (let ((ordered (causal-self-walker--chain-ordered)))
      (should (= 3 (length ordered)))
      (should (= 0 (causal-self-walker-state-step (nth 0 ordered))))
      (should (= 1 (causal-self-walker-state-step (nth 1 ordered))))
      (should (= 2 (causal-self-walker-state-step (nth 2 ordered)))))))

;;; ── Seed evolution ───────────────────────────────────────────────────────

(ert-deftest test-self-walker-seed-evolves ()
  "Seed changes after recording a state."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 42))
    (let ((before causal-self-walker--seed))
      (causal-self-walker--record-state 0 nil "g" "" "" 'initial)
      (should-not (= before causal-self-walker--seed)))))

;;; ── Formatting ───────────────────────────────────────────────────────────

(ert-deftest test-self-walker-format-hyps ()
  "Format hypotheses as comma-separated context."
  (let ((result (causal-self-walker--format-hyps
                 '(("h1" . "Nat") ("h2" . "Bool")))))
    (should (string-match-p "h1 : Nat" result))
    (should (string-match-p "h2 : Bool" result))
    (should (string-match-p ", " result))))

(ert-deftest test-self-walker-format-hyps-empty ()
  "Formatting empty hyps returns empty string."
  (should (string= "" (causal-self-walker--format-hyps nil))))

(ert-deftest test-self-walker-format-state ()
  "Format a state struct as human-readable string."
  (let* ((s (causal-self-walker-state-create
             :step 1 :hypotheses '(("h" . "Nat"))
             :goal "h > 0" :tactic "intro" :source-fn "fn"
             :status 'active))
         (result (causal-self-walker--format-state s)))
    (should (string-match-p "State 1:" result))
    (should (string-match-p "⊢ h > 0" result))
    (should (string-match-p "tactic: intro" result))))

(ert-deftest test-self-walker-format-chain ()
  "Format a full chain as a summary string."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 0))
    (causal-self-walker--record-state 0 nil "g" "" "" 'initial)
    (causal-self-walker--record-state 1 nil "" "intro" "fn" 'qed)
    (let ((result (causal-self-walker--format-chain)))
      (should (string-match-p "State Chain (2 steps)" result))
      (should (string-match-p "Seed:" result)))))

;;; ── Olog generators ──────────────────────────────────────────────────────

(ert-deftest test-self-walker-chain-to-ob-generators ()
  "Chain converts to CatColab ob_generators vector."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 0))
    (causal-self-walker--record-state 0 '(("h" . "Nat")) "h = 0" "" "" 'initial)
    (causal-self-walker--record-state 1 nil "No Goals" "" "" 'qed)
    (let* ((chain (causal-self-walker--chain-ordered))
           (obs (causal-self-walker--chain-to-ob-generators chain)))
      (should (vectorp obs))
      (should (= 2 (length obs)))
      (should (string= "state-0"
                        (alist-get "id" (aref obs 0) nil nil #'string=)))
      (should (string= "state-1"
                        (alist-get "id" (aref obs 1) nil nil #'string=))))))

(ert-deftest test-self-walker-chain-to-mor-generators ()
  "Chain converts to CatColab mor_generators vector."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 0))
    (causal-self-walker--record-state 0 nil "g" "" "" 'initial)
    (causal-self-walker--record-state 1 nil "" "intro" "fn" 'qed)
    (let* ((chain (causal-self-walker--chain-ordered))
           (mors (causal-self-walker--chain-to-mor-generators chain)))
      (should (vectorp mors))
      (should (= 1 (length mors)))
      (should (string= "step-0-to-1"
                        (alist-get "id" (aref mors 0) nil nil #'string=))))))

(ert-deftest test-self-walker-single-state-no-morphisms ()
  "A single-state chain produces no morphisms."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 0))
    (causal-self-walker--record-state 0 nil "g" "" "" 'initial)
    (let* ((chain (causal-self-walker--chain-ordered))
           (mors (causal-self-walker--chain-to-mor-generators chain)))
      (should (vectorp mors))
      (should (= 0 (length mors))))))

;;; ── Reset ────────────────────────────────────────────────────────────────

(ert-deftest test-self-walker-reset ()
  "Reset clears chain, seed, and vocab."
  (let ((causal-self-walker--chain '(dummy))
        (causal-self-walker--seed 999)
        (causal-self-walker--tactic-vocab '("x")))
    (causal-self-walker-reset)
    (should (null causal-self-walker--chain))
    (should (= 0 causal-self-walker--seed))
    (should (null causal-self-walker--tactic-vocab))))

;;; ── Customization defaults ───────────────────────────────────────────────

(ert-deftest test-self-walker-default-max-steps ()
  "Default max-steps is 50."
  (should (= 50 causal-self-walker-max-steps)))

(ert-deftest test-self-walker-default-max-retries ()
  "Default max-retries is 3."
  (should (= 3 causal-self-walker-max-retries)))

(ert-deftest test-self-walker-default-auto-export ()
  "Default auto-export is t."
  (should causal-self-walker-auto-export))

(provide 'test-causal-self-walker)
;;; test-causal-self-walker.el ends here
