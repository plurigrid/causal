;;; test-causal-self-walker-mc.el --- ERT tests for abductive MC tactic proposal -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools

;;; Commentary:

;; Offline ERT tests for causal-self-walker-mc.el.
;; Covers SplitMix64 RNG, prior scoring, complexity, sampling,
;; Metropolis-Hastings acceptance, and GF(3) trit assignment.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'causal-self-walker)
;; MC is loaded via optional require in self-walker; ensure it's loaded.
(unless (featurep 'causal-self-walker-mc)
  (require 'causal-self-walker-mc))

;;; ── Feature loading ──────────────────────────────────────────────────────

(ert-deftest test-mc-loads ()
  "causal-self-walker-mc feature is available."
  (should (featurep 'causal-self-walker-mc)))

;;; ── SplitMix64 determinism ──────────────────────────────────────────────

(ert-deftest test-mc-splitmix64-deterministic ()
  "Same seed produces same output."
  (let ((a (causal-self-walker-mc--splitmix64 42))
        (b (causal-self-walker-mc--splitmix64 42)))
    (should (= (car a) (car b)))
    (should (= (cdr a) (cdr b)))))

(ert-deftest test-mc-splitmix64-different-seeds ()
  "Different seeds produce different outputs."
  (let ((a (causal-self-walker-mc--splitmix64 42))
        (b (causal-self-walker-mc--splitmix64 99)))
    (should-not (= (car a) (car b)))))

(ert-deftest test-mc-splitmix64-advances-seed ()
  "Next-seed differs from input seed."
  (let ((pair (causal-self-walker-mc--splitmix64 42)))
    (should-not (= 42 (cdr pair)))))

(ert-deftest test-mc-random-float-range ()
  "Random float is in [0, 1]."
  (let ((seed 12345))
    (dotimes (_ 20)
      (let ((pair (causal-self-walker-mc--random-float seed)))
        (should (>= (car pair) 0.0))
        (should (<= (car pair) 1.0))
        (setq seed (cdr pair))))))

(ert-deftest test-mc-random-index-range ()
  "Random index is in [0, N)."
  (let ((seed 12345)
        (n 7))
    (dotimes (_ 20)
      (let ((pair (causal-self-walker-mc--random-index seed n)))
        (should (>= (car pair) 0))
        (should (< (car pair) n))
        (setq seed (cdr pair))))))

;;; ── Prior scoring ────────────────────────────────────────────────────────

(ert-deftest test-mc-prior-intro-forall ()
  "intro scores high when goal contains forall."
  (let ((score (causal-self-walker-mc--prior-score "intro" "∀ x : Nat, x = x")))
    (should (> score 0.1))))

(ert-deftest test-mc-prior-simp-arithmetic ()
  "simp scores high when goal contains arithmetic."
  (let ((score (causal-self-walker-mc--prior-score "simp" "n + 0 = n")))
    (should (> score 0.1))))

(ert-deftest test-mc-prior-unrelated ()
  "Tactic with no keyword match returns baseline 0.1."
  (let ((score (causal-self-walker-mc--prior-score "unfold" "∀ x, x > 0")))
    (should (= score 0.1))))

(ert-deftest test-mc-prior-multiple-matches ()
  "Score increases with multiple keyword matches."
  (let ((single (causal-self-walker-mc--prior-score "omega" "Nat"))
        (multi (causal-self-walker-mc--prior-score "omega" "Nat ≤ ℕ")))
    (should (>= multi single))))

(ert-deftest test-mc-prior-nil-goal ()
  "Nil goal returns baseline 0.1 for any tactic."
  (should (= 0.1 (causal-self-walker-mc--prior-score "intro" nil))))

;;; ── Goal complexity ──────────────────────────────────────────────────────

(ert-deftest test-mc-complexity-empty ()
  "Empty goal has zero complexity."
  (should (= 0.0 (causal-self-walker-mc--goal-complexity ""))))

(ert-deftest test-mc-complexity-nil ()
  "Nil goal has zero complexity."
  (should (= 0.0 (causal-self-walker-mc--goal-complexity nil))))

(ert-deftest test-mc-complexity-ordering ()
  "More complex goal has higher complexity score."
  (let ((simple (causal-self-walker-mc--goal-complexity "x = 0"))
        (complex (causal-self-walker-mc--goal-complexity
                  "(∀ x → (∃ y → (f(x) = g(y))))")))
    (should (> complex simple))))

(ert-deftest test-mc-complexity-parens-increase ()
  "Parentheses increase complexity."
  (let ((no-parens (causal-self-walker-mc--goal-complexity "x = 0"))
        (parens (causal-self-walker-mc--goal-complexity "(x) = (0)")))
    (should (> parens no-parens))))

;;; ── Abductive score ──────────────────────────────────────────────────────

(ert-deftest test-mc-abductive-score-positive ()
  "Abductive score is always positive."
  (let ((score (causal-self-walker-mc--abductive-score "intro" "∀ x, P x" nil)))
    (should (> score 0.0))))

(ert-deftest test-mc-abductive-history-bonus ()
  "History-used tactic gets a bonus."
  (let ((without (causal-self-walker-mc--abductive-score "intro" "∀ x" nil))
        (with (causal-self-walker-mc--abductive-score "intro" "∀ x" '("intro"))))
    (should (> with without))))

;;; ── Normalize weights ────────────────────────────────────────────────────

(ert-deftest test-mc-normalize-sums-to-one ()
  "Normalized weights sum to 1.0."
  (let* ((weights '(1.0 2.0 3.0 4.0))
         (normed (causal-self-walker-mc--normalize-weights weights)))
    (should (< (abs (- 1.0 (apply #'+ normed))) 1e-10))))

(ert-deftest test-mc-normalize-zero-weights ()
  "All-zero weights produce uniform distribution."
  (let* ((weights '(0.0 0.0 0.0))
         (normed (causal-self-walker-mc--normalize-weights weights)))
    (dolist (w normed)
      (should (< (abs (- w (/ 1.0 3.0))) 1e-10)))))

;;; ── Weighted sampling ────────────────────────────────────────────────────

(ert-deftest test-mc-weighted-sample-returns-item ()
  "Weighted sample returns an item from the list."
  (let* ((items '("a" "b" "c"))
         (weights '(0.33 0.34 0.33))
         (result (causal-self-walker-mc--weighted-sample items weights 42)))
    (should (member (car result) items))))

(ert-deftest test-mc-weighted-sample-deterministic ()
  "Same seed produces same sample."
  (let* ((items '("a" "b" "c"))
         (weights '(0.33 0.34 0.33))
         (r1 (causal-self-walker-mc--weighted-sample items weights 42))
         (r2 (causal-self-walker-mc--weighted-sample items weights 42)))
    (should (string= (car r1) (car r2)))))

;;; ── Proposal engine ──────────────────────────────────────────────────────

(ert-deftest test-mc-propose-returns-sorted ()
  "Proposals are sorted by descending score."
  (let* ((vocab '("intro" "apply" "simp" "ring"))
         (proposals (causal-self-walker-mc-propose "∀ x, x + 0 = x" vocab 42)))
    (should (listp proposals))
    (should (= (length vocab) (length proposals)))
    (let ((scores (mapcar #'cdr proposals)))
      (should (equal scores (sort (copy-sequence scores) #'>))))))

(ert-deftest test-mc-propose-all-positive-scores ()
  "All proposal scores are positive."
  (let* ((vocab '("intro" "apply" "exact" "rewrite" "simp"))
         (proposals (causal-self-walker-mc-propose "n = n" vocab 42)))
    (dolist (p proposals)
      (should (> (cdr p) 0.0)))))

;;; ── Sample-one / sample-n ────────────────────────────────────────────────

(ert-deftest test-mc-sample-one-in-vocab ()
  "sample-one returns an item from the vocabulary."
  (let* ((vocab '("intro" "apply" "exact" "simp"))
         (result (causal-self-walker-mc-sample-one "x = x" vocab 42)))
    (should (member (car result) vocab))))

(ert-deftest test-mc-sample-n-counts ()
  "sample-n returns counts that sum to N."
  (let* ((vocab '("intro" "apply" "exact" "simp"))
         (results (causal-self-walker-mc-sample-n "x = x" vocab 42 100)))
    (should (= 100 (apply #'+ (mapcar #'cdr results))))))

;;; ── Metropolis-Hastings ──────────────────────────────────────────────────

(ert-deftest test-mc-mh-always-accept-improvement ()
  "MH always accepts when new >= old."
  (let ((result (causal-self-walker-mc--mh-accept-p 0.5 0.8 42)))
    (should (eq t (car result)))))

(ert-deftest test-mc-mh-sometimes-reject-degradation ()
  "MH sometimes rejects when new < old (ratio-based)."
  (let ((accepted 0)
        (seed 0))
    (dotimes (_ 100)
      (let ((result (causal-self-walker-mc--mh-accept-p 1.0 0.01 seed)))
        (when (car result) (cl-incf accepted))
        (setq seed (cdr result))))
    (should (< accepted 50))))

(ert-deftest test-mc-mh-zero-old-always-accepts ()
  "MH accepts when old-score is zero (ratio defaults to 1.0)."
  (let ((result (causal-self-walker-mc--mh-accept-p 0.0 0.5 42)))
    (should (eq t (car result)))))

;;; ── GF(3) trit assignment ────────────────────────────────────────────────

(ert-deftest test-mc-trit-generative ()
  "Generative tactics get trit +1."
  (dolist (tac '("intro" "cases" "induction" "destruct"))
    (should (= 1 (causal-self-walker-mc-tactic-trit tac)))))

(ert-deftest test-mc-trit-reductive ()
  "Reductive tactics get trit -1."
  (dolist (tac '("simp" "ring" "omega" "rfl" "exact"))
    (should (= -1 (causal-self-walker-mc-tactic-trit tac)))))

(ert-deftest test-mc-trit-mediating ()
  "Mediating tactics get trit 0."
  (dolist (tac '("apply" "rewrite" "unfold"))
    (should (= 0 (causal-self-walker-mc-tactic-trit tac)))))

(ert-deftest test-mc-walk-trit-sum-empty ()
  "Trit sum of empty chain is 0."
  (let ((causal-self-walker--chain nil))
    (should (= 0 (causal-self-walker-mc-walk-trit-sum)))))

(ert-deftest test-mc-walk-trit-sum-balanced ()
  "Trit sum of intro+simp chain is (1 + -1) mod 3 = 0."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 0))
    (causal-self-walker--record-state 0 nil "g" "" "" 'initial)
    (causal-self-walker--record-state 1 nil "g2" "intro" "fn" 'active)
    (causal-self-walker--record-state 2 nil "" "simp" "fn" 'qed)
    (should (= 0 (causal-self-walker-mc-walk-trit-sum)))))

;;; ── Temperature ──────────────────────────────────────────────────────────

(ert-deftest test-mc-temperature-default ()
  "Default temperature is 1.0."
  (should (= 1.0 causal-self-walker-mc-temperature)))

(ert-deftest test-mc-temperature-low-concentrates ()
  "Low temperature concentrates probability on best tactic."
  (let* ((vocab '("intro" "apply" "simp" "ring"))
         (causal-self-walker-mc-temperature 0.01)
         (proposals (causal-self-walker-mc-propose "∀ x, x = x" vocab 42))
         (top-score (cdr (car proposals)))
         (rest-scores (mapcar #'cdr (cdr proposals))))
    (dolist (s rest-scores)
      (should (> top-score s)))))

(ert-deftest test-mc-n-samples-default ()
  "Default n-samples is 100."
  (should (= 100 causal-self-walker-mc-n-samples)))

(provide 'test-causal-self-walker-mc)
;;; test-causal-self-walker-mc.el ends here
