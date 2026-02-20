;;; causal-self-walker-mc.el --- Abductive Monte Carlo tactic proposal -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, languages
;; URL: https://github.com/plurigrid/causal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Abductive Monte Carlo tactic proposal engine for causal-self-walker.
;;
;; Given a proof state (Gamma |- G), this module proposes the most likely
;; next tactic via importance-weighted MCMC over the tactic vocabulary.
;;
;; Scoring:
;;   P(tactic | goal) ∝ P(goal | tactic) · P(tactic)
;;   - Prior P(tactic): keyword heuristics + frequency from history
;;   - Likelihood P(goal|tactic): goal complexity reduction after step
;;
;; GF(3) triad:
;;   abductive-monte-carlo (-1) ⊗ self-walker (0) ⊗ gay-monte-carlo (+1) = 0
;;
;; QUICK START
;;   (require 'causal-self-walker-mc)
;;   (causal-self-walker-mc-propose "n + 0 = n" vocab seed)

;;; Code:

(require 'cl-lib)

;; Avoid circular require: self-walker optionally loads us,
;; so we use declare-function for the few symbols we reference.
(declare-function causal-self-walker-state-goal "causal-self-walker")
(declare-function causal-self-walker-state-tactic "causal-self-walker")
(declare-function causal-self-walker-tactic-vocab "causal-self-walker")
(declare-function causal-self-walker--chain-ordered "causal-self-walker")
(declare-function causal-self-walker--scan-goal "causal-self-walker")
(defvar causal-self-walker--chain)
(defvar causal-self-walker--seed)

;;; Customization

(defgroup causal-self-walker-mc nil
  "Abductive Monte Carlo tactic proposal."
  :group 'causal-self-walker
  :prefix "causal-self-walker-mc-")

(defcustom causal-self-walker-mc-n-samples 100
  "Number of MCMC samples per proposal round."
  :type 'integer
  :group 'causal-self-walker-mc)

(defcustom causal-self-walker-mc-temperature 1.0
  "Sampling temperature. Lower = more greedy, higher = more exploratory."
  :type 'float
  :group 'causal-self-walker-mc)

;;; SplitMix64 RNG (deterministic, splittable)

(defun causal-self-walker-mc--splitmix64 (seed)
  "SplitMix64 step: return (next-value . next-seed).
Faithful port of the Java SplitMix64 algorithm to Elisp bignum."
  (let* ((s (logand (+ seed #x9E3779B97F4A7C15) #xFFFFFFFFFFFFFFFF))
         (z s)
         (z (logand (logxor z (ash z -30)) #xFFFFFFFFFFFFFFFF))
         (z (logand (* z #xBF58476D1CE4E5B9) #xFFFFFFFFFFFFFFFF))
         (z (logand (logxor z (ash z -27)) #xFFFFFFFFFFFFFFFF))
         (z (logand (* z #x94D049BB133111EB) #xFFFFFFFFFFFFFFFF))
         (z (logand (logxor z (ash z -31)) #xFFFFFFFFFFFFFFFF)))
    (cons z s)))

(defun causal-self-walker-mc--random-float (seed)
  "Return (float-in-0-1 . next-seed) from SEED via SplitMix64."
  (let* ((pair (causal-self-walker-mc--splitmix64 seed))
         (val (car pair))
         (next (cdr pair)))
    (cons (/ (float (logand val #x1FFFFFFFFFFFFF)) (float #x1FFFFFFFFFFFFF))
          next)))

(defun causal-self-walker-mc--random-index (seed n)
  "Return (index-in-0-to-N-1 . next-seed) from SEED."
  (let* ((pair (causal-self-walker-mc--splitmix64 seed))
         (val (car pair))
         (next (cdr pair)))
    (cons (mod val n) next)))

;;; Tactic Prior (keyword heuristics)

(defconst causal-self-walker-mc--tactic-keywords
  '(("intro"     . ("forall" "∀" "→" "->" "fun" "λ" "Π"))
    ("apply"     . (":" "have" "lemma" "theorem"))
    ("exact"     . ("=" "rfl" "refl" "trivial"))
    ("rewrite"   . ("=" "≡" "eq" "rewrite" "subst"))
    ("simp"      . ("+" "-" "*" "/" "nat" "int" "Nat" "Int" "0" "1" "succ"))
    ("ring"      . ("+" "*" "^" "ring" "comm" "assoc"))
    ("omega"     . ("<" ">" "≤" "≥" "<=" ">=" "Nat" "Int" "ℕ" "ℤ"))
    ("cases"     . ("∨" "or" "Or" "match" "inl" "inr" "Sum" "Bool"))
    ("induction" . ("Nat" "ℕ" "List" "succ" "rec" "ind"))
    ("rfl"       . ("=" "refl" "Eq"))
    ("destruct"  . ("∧" "and" "And" "Prod" "Sigma" "∃" "exists"))
    ("unfold"    . ("def" "definition" "let" "where")))
  "Keyword hints mapping tactics to goal substrings that suggest their use.")

(defun causal-self-walker-mc--prior-score (tactic goal)
  "Compute prior score for TACTIC given GOAL string.
Returns a float in [0.1, 1.0]."
  (let ((keywords (cdr (assoc tactic causal-self-walker-mc--tactic-keywords)))
        (score 0.1))
    (when keywords
      (dolist (kw keywords)
        (when (string-match-p (regexp-quote kw) (or goal ""))
          (setq score (min 1.0 (+ score 0.3))))))
    score))

;;; Goal Complexity

(defun causal-self-walker-mc--goal-complexity (goal)
  "Estimate complexity of GOAL string.
Simpler goals are shorter with fewer nested symbols."
  (if (or (null goal) (string-empty-p goal))
      0.0
    (let ((len (float (length goal)))
          (parens (float (cl-count ?\( goal)))
          (arrows (float (cl-count ?→ goal))))
      (+ len (* 2.0 parens) (* 3.0 arrows)))))

;;; Abductive Score

(defun causal-self-walker-mc--abductive-score (tactic goal history)
  "Score TACTIC as explanation for reaching GOAL given HISTORY.
Combines prior (keyword match) with history frequency bonus."
  (let* ((prior (causal-self-walker-mc--prior-score tactic goal))
         (freq-bonus (if (member tactic history) 0.1 0.0))
         (complexity (causal-self-walker-mc--goal-complexity goal))
         (occam (/ 1.0 (+ 1.0 (log (+ 1.0 complexity))))))
    (* (+ prior freq-bonus) occam)))

;;; Weighted Sampling

(defun causal-self-walker-mc--normalize-weights (weights)
  "Normalize WEIGHTS (list of floats) to sum to 1.0."
  (let ((total (apply #'+ weights)))
    (if (zerop total)
        (make-list (length weights) (/ 1.0 (float (length weights))))
      (mapcar (lambda (w) (/ w total)) weights))))

(defun causal-self-walker-mc--weighted-sample (items weights seed)
  "Sample one item from ITEMS with WEIGHTS using SEED.
Returns (item . next-seed)."
  (let* ((pair (causal-self-walker-mc--random-float seed))
         (u (car pair))
         (next-seed (cdr pair))
         (cumsum 0.0)
         (result (car items)))
    (cl-loop for item in items
             for w in weights
             do (setq cumsum (+ cumsum w))
             when (< u cumsum) return (setq result item))
    (cons result next-seed)))

;;; Proposal Engine

(defun causal-self-walker-mc--score-all (vocab goal history)
  "Score all tactics in VOCAB for GOAL with HISTORY.
Returns list of (tactic . score) pairs."
  (mapcar (lambda (tac)
            (cons tac (causal-self-walker-mc--abductive-score tac goal history)))
          vocab))

(defun causal-self-walker-mc--apply-temperature (scores temperature)
  "Apply TEMPERATURE to SCORES list, returning adjusted weights.
Lower temperature = more peaked distribution."
  (let ((max-score (apply #'max (mapcar #'cdr scores))))
    (mapcar (lambda (entry)
              (let* ((s (cdr entry))
                     (adjusted (exp (/ (- s max-score) (max temperature 0.01)))))
                (cons (car entry) adjusted)))
            scores)))

(defun causal-self-walker-mc-propose (goal vocab seed &optional history)
  "Propose top tactics for GOAL from VOCAB using abductive MCMC.
SEED is the RNG seed. HISTORY is list of previously used tactics.
Returns an ordered list of (tactic . score) pairs."
  (let* ((raw-scores (causal-self-walker-mc--score-all vocab goal history))
         (tempered (causal-self-walker-mc--apply-temperature
                    raw-scores causal-self-walker-mc-temperature))
         (sorted (sort tempered (lambda (a b) (> (cdr a) (cdr b))))))
    sorted))

(defun causal-self-walker-mc-sample-one (goal vocab seed &optional history)
  "Sample a single tactic from the proposal distribution.
Returns (tactic . next-seed)."
  (let* ((scores (causal-self-walker-mc-propose goal vocab seed history))
         (tactics (mapcar #'car scores))
         (weights (causal-self-walker-mc--normalize-weights
                   (mapcar #'cdr scores))))
    (causal-self-walker-mc--weighted-sample tactics weights seed)))

(defun causal-self-walker-mc-sample-n (goal vocab seed n &optional history)
  "Sample N tactics from the proposal distribution.
Returns list of (tactic . count) pairs with duplicates collapsed."
  (let ((counts (make-hash-table :test 'equal))
        (current-seed seed))
    (dotimes (_ n)
      (let* ((result (causal-self-walker-mc-sample-one
                      goal vocab current-seed history))
             (tactic (car result)))
        (setq current-seed (cdr result))
        (puthash tactic (1+ (gethash tactic counts 0)) counts)))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) counts)
      (sort result (lambda (a b) (> (cdr a) (cdr b)))))))

;;; Metropolis-Hastings Accept/Reject

(defun causal-self-walker-mc--mh-accept-p (old-score new-score seed)
  "Metropolis-Hastings acceptance criterion.
Returns (accepted-p . next-seed)."
  (if (>= new-score old-score)
      (cons t seed)
    (let* ((pair (causal-self-walker-mc--random-float seed))
           (u (car pair))
           (next-seed (cdr pair))
           (ratio (if (zerop old-score) 1.0 (/ new-score old-score))))
      (cons (< u ratio) next-seed))))

;;; Walk Integration

(defun causal-self-walker-mc-propose-for-state (state seed)
  "Propose ranked tactics for a self-walker STATE.
Returns list of (tactic . score) pairs."
  (let* ((goal (causal-self-walker-state-goal state))
         (vocab (causal-self-walker-tactic-vocab))
         (history (mapcar #'causal-self-walker-state-tactic
                          causal-self-walker--chain)))
    (causal-self-walker-mc-propose goal vocab seed history)))

;;; Display

(defun causal-self-walker-mc--format-proposals (proposals &optional top-k)
  "Format PROPOSALS as a display string, showing TOP-K entries."
  (let* ((k (or top-k 10))
         (shown (cl-subseq proposals 0 (min k (length proposals)))))
    (concat
     (format "Abductive MC Proposals (top %d of %d)\n"
             (length shown) (length proposals))
     (make-string 40 ?─) "\n\n"
     (mapconcat
      (lambda (entry)
        (format "  %-15s  score=%.4f" (car entry) (cdr entry)))
      shown "\n")
     "\n")))

;;;###autoload
(defun causal-self-walker-mc-show-proposals ()
  "Display MC tactic proposals for the current proof state."
  (interactive)
  (let* ((goal (causal-self-walker--scan-goal))
         (vocab (causal-self-walker-tactic-vocab))
         (proposals (causal-self-walker-mc-propose
                     goal vocab causal-self-walker--seed)))
    (with-current-buffer (get-buffer-create "*self-walker: MC proposals*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Goal: %s\n\n" (or goal "(none)")))
        (insert (causal-self-walker-mc--format-proposals proposals)))
      (view-mode 1)
      (goto-char (point-min)))
    (display-buffer "*self-walker: MC proposals*")))

;;; GF(3) Trit Assignment

(defun causal-self-walker-mc-tactic-trit (tactic)
  "Assign a GF(3) trit to TACTIC based on its structural role.
+1: generative (intro, cases, induction, destruct)
-1: reductive (simp, ring, omega, rfl, exact)
 0: mediating (apply, rewrite, unfold)"
  (cond
   ((member tactic '("intro" "cases" "induction" "destruct")) 1)
   ((member tactic '("simp" "ring" "omega" "rfl" "exact")) -1)
   (t 0)))

(defun causal-self-walker-mc-walk-trit-sum ()
  "Compute the GF(3) trit sum (mod 3) of the current walk chain."
  (let ((chain (causal-self-walker--chain-ordered)))
    (mod (apply #'+ (mapcar (lambda (s)
                              (causal-self-walker-mc-tactic-trit
                               (causal-self-walker-state-tactic s)))
                            chain))
         3)))

(provide 'causal-self-walker-mc)
;;; causal-self-walker-mc.el ends here
