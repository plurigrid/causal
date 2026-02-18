;;; causal-proofreader.el --- Transient UI for Proof Assistants -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Causal ProofReader is an opinionated Transient-based porcelain for
;; proof assistants including Proof General (Coq, Narya), Lean 4, and
;; Narya standalone.
;;
;; INSTALLATION
;; (require 'causal-proofreader) ; optional if using autoloaded menu
;;
;; For Proof General modes (Coq, Narya):
;; (with-eval-after-load 'proof-general
;;   (keymap-set proof-mode-map "C-o" #'causal-proofreader-tmenu))
;;
;; For Lean 4:
;; (with-eval-after-load 'lean4-mode
;;   (keymap-set lean4-mode-map "C-o" #'causal-proofreader-tmenu))

;;; Code:
(require 'transient)
(require 'causal-lib)
(require 'causal-proofreader-utils)
(require 'causal-proofreader-settings)

;;; Proof Navigation Commands (backend-dispatching wrappers)

(defun causal-proofreader-step-forward ()
  "Step forward one proof command."
  (interactive)
  (causal-proofreader--dispatch
   #'proof-assert-next-command-interactive
   #'lean4-execute
   #'proof-assert-next-command-interactive))

(defun causal-proofreader-step-backward ()
  "Step backward one proof command."
  (interactive)
  (causal-proofreader--dispatch
   #'proof-undo-last-successful-command
   #'lean4-undo
   #'proof-undo-last-successful-command))

(defun causal-proofreader-goto-point ()
  "Process proof up to point."
  (interactive)
  (causal-proofreader--dispatch
   #'proof-goto-point
   #'lean4-refresh-file-dependencies
   #'proof-goto-point))

(defun causal-proofreader-process-buffer ()
  "Process entire buffer."
  (interactive)
  (causal-proofreader--dispatch
   #'proof-process-buffer
   #'lean4-refresh-file-dependencies
   #'proof-process-buffer))

(defun causal-proofreader-retract ()
  "Retract to beginning of buffer."
  (interactive)
  (causal-proofreader--dispatch
   #'proof-retract-buffer
   #'lean4-undo
   #'proof-retract-buffer))

(defun causal-proofreader-goto-end-of-locked ()
  "Jump to end of locked region."
  (interactive)
  (causal-proofreader--dispatch
   #'proof-goto-end-of-locked
   #'beginning-of-buffer
   #'proof-goto-end-of-locked))

;;; Goal Commands

(defun causal-proofreader-show-goals ()
  "Display current goals."
  (interactive)
  (causal-proofreader--dispatch
   #'proof-display-some-buffers
   #'lean4-show-goal
   #'proof-display-some-buffers))

(defun causal-proofreader-show-context ()
  "Display current context/hypotheses."
  (interactive)
  (causal-proofreader--dispatch
   #'proof-display-some-buffers
   #'lean4-show-goal
   #'proof-display-some-buffers))

;;; Hole Commands

(defun causal-proofreader-next-hole ()
  "Jump to next typed hole."
  (interactive)
  (search-forward "?" nil t))

(defun causal-proofreader-prev-hole ()
  "Jump to previous typed hole."
  (interactive)
  (search-backward "?" nil t))

;;; Search Commands

(defun causal-proofreader-search-lemma ()
  "Search for a lemma/theorem."
  (interactive)
  (causal-proofreader--dispatch
   #'coq-SearchAbout
   #'lean4-find-definition
   #'proof-find-theorems
   "Lemma search not available for this backend."))

;;; Tactic Insertion

(defun causal-proofreader--insert-tactic (tactic)
  "Insert TACTIC text at point."
  (insert tactic)
  (insert " "))

(defun causal-proofreader-tactic-intro ()
  "Insert `intro' tactic." (interactive)
  (causal-proofreader--insert-tactic "intro"))

(defun causal-proofreader-tactic-apply ()
  "Insert `apply' tactic." (interactive)
  (causal-proofreader--insert-tactic "apply"))

(defun causal-proofreader-tactic-exact ()
  "Insert `exact' tactic." (interactive)
  (causal-proofreader--insert-tactic "exact"))

(defun causal-proofreader-tactic-rewrite ()
  "Insert `rewrite' tactic." (interactive)
  (causal-proofreader--insert-tactic "rewrite"))

(defun causal-proofreader-tactic-simp ()
  "Insert `simp' tactic." (interactive)
  (causal-proofreader--insert-tactic "simp"))

(defun causal-proofreader-tactic-ring ()
  "Insert `ring' tactic." (interactive)
  (causal-proofreader--insert-tactic "ring"))

(defun causal-proofreader-tactic-omega ()
  "Insert `omega' tactic." (interactive)
  (causal-proofreader--insert-tactic "omega"))

(defun causal-proofreader-tactic-cases ()
  "Insert `cases' tactic." (interactive)
  (causal-proofreader--insert-tactic "cases"))

(defun causal-proofreader-tactic-induction ()
  "Insert `induction' tactic." (interactive)
  (causal-proofreader--insert-tactic "induction"))

(defun causal-proofreader-tactic-rfl ()
  "Insert `rfl' tactic." (interactive)
  (causal-proofreader--insert-tactic "rfl"))

;;; Window Layout

(defun causal-proofreader-layout-three ()
  "Set three-window layout (script | goals | response)."
  (interactive)
  (when (fboundp 'proof-layout-windows)
    (proof-layout-windows)))

(defun causal-proofreader-layout-two ()
  "Set two-window layout (script | goals)."
  (interactive)
  (delete-other-windows)
  (when (and (fboundp 'proof-display-some-buffers)
             (causal-proofreader--proof-general-p))
    (split-window-right)
    (other-window 1)
    (proof-display-some-buffers)
    (other-window -1)))

;;; Sub-menus

(transient-define-prefix causal-proofreader-tactics-tmenu ()
  "Tactic dispatch menu."
  ["Introduce & Eliminate"
   ("i" "intro" causal-proofreader-tactic-intro)
   ("a" "apply" causal-proofreader-tactic-apply)
   ("e" "exact" causal-proofreader-tactic-exact)
   ("c" "cases" causal-proofreader-tactic-cases)
   ("I" "induction" causal-proofreader-tactic-induction)]

  ["Simplify & Solve"
   ("s" "simp" causal-proofreader-tactic-simp)
   ("r" "ring" causal-proofreader-tactic-ring)
   ("o" "omega" causal-proofreader-tactic-omega)
   ("=" "rfl" causal-proofreader-tactic-rfl)
   ("w" "rewrite" causal-proofreader-tactic-rewrite)]

  [:class transient-row
   (causal-lib-quit-one)])

(transient-define-prefix causal-proofreader-holes-tmenu ()
  "Hole navigation menu."
  ["Holes"
   ("n" "Next hole" causal-proofreader-next-hole
    :description (lambda () (causal-proofreader-unicode-get :down-arrow))
    :transient t)
   ("p" "Prev hole" causal-proofreader-prev-hole
    :description (lambda () (causal-proofreader-unicode-get :up-arrow))
    :transient t)]

  [:class transient-row
   (causal-lib-quit-one)])

(transient-define-prefix causal-proofreader-window-tmenu ()
  "Window layout menu."
  ["Layout"
   ("3" "Three windows" causal-proofreader-layout-three)
   ("2" "Two windows" causal-proofreader-layout-two)
   ("1" "Single window" delete-other-windows)]

  [:class transient-row
   (causal-lib-quit-one)])

;;; Main Menu
;;;###autoload (autoload 'causal-proofreader-tmenu "causal-proofreader" nil t)

(transient-define-prefix causal-proofreader-tmenu ()
  "Causal ProofReader Transient menu."

  [:description
   (lambda () (format "ProofReader [%s]"
                      (causal-proofreader--backend-name)))]

  [["Proof Navigation"
    ("n" "Step forward" causal-proofreader-step-forward
     :description (lambda () (causal-proofreader-unicode-get :step-forward))
     :transient t)
    ("p" "Step backward" causal-proofreader-step-backward
     :description (lambda () (causal-proofreader-unicode-get :step-backward))
     :transient t)
    ("RET" "To point" causal-proofreader-goto-point
     :description (lambda () (causal-proofreader-unicode-get :to-point)))
    ("b" "Process buffer" causal-proofreader-process-buffer)
    ("." "End of locked" causal-proofreader-goto-end-of-locked
     :description (lambda () (causal-proofreader-unicode-get :lock))
     :transient t)
    ("r" "Retract" causal-proofreader-retract
     :description (lambda () (causal-proofreader-unicode-get :retract)))]

   ["Goals & Holes"
    ("g" "Show goals" causal-proofreader-show-goals
     :description (lambda () (causal-proofreader-unicode-get :goal))
     :transient t)
    ("c" "Show context" causal-proofreader-show-context
     :description (lambda () (causal-proofreader-unicode-get :context))
     :transient t)
    ("h" "Holes›" causal-proofreader-holes-tmenu)
    ("t" "Tactics›" causal-proofreader-tactics-tmenu)]]

  [["Search"
    ("s" "Search lemma…" causal-proofreader-search-lemma)
    ("/" "I-search…" isearch-forward)
    ("i" "Imenu…" imenu)]

   ["Window"
    ("w" "Layout›" causal-proofreader-window-tmenu)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("," "Settings›" causal-proofreader-settings-tmenu)
   (causal-lib-quit-all)])

(provide 'causal-proofreader)
;;; causal-proofreader.el ends here
