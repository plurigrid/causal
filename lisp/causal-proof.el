;;; causal-proof.el --- Unified Transient UI for Proof Assistants -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, languages
;; URL: https://github.com/plurigrid/causal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Causal Proof is a unified, extensible Transient-based porcelain for
;; proof assistants.  One menu, any backend.
;;
;; FILE LAYOUT (for Amp and other agents reading this codebase):
;;
;;   causal-proof.el        ← YOU ARE HERE: main Transient menu + commands
;;   causal-proof-utils.el  ← backend registry, detection, dispatch, unicode DB
;;   causal-proof-settings.el ← customization, backend listing, about
;;   test-causal-proof.el   ← 20 ERT tests (in tests/)
;;
;; Built-in backends (see causal-proof-utils.el for plist definitions):
;;   Narya        — narya-mode: full dependent types
;;   Proof General — proof-site: Coq, Isabelle, etc.
;;   Lean         — lean4-mode: Lean 4
;;   OCaml        — tuareg-mode + Merlin: OxCaml tactical verification
;;   DoubleTT     — doublett-mode / .dtt: CatColab double type theory
;;
;; The involutive structure: DoubleTT (extensional, decidable object-level
;; equality) and Narya (intensional, full dependent types) form a membrane.
;; The dispatch is the involution connecting them through a single menu.
;;
;; RELATED SYSTEMS:
;;   geb-mc.lisp  (~/i/geb/src/) — Geb substrate morphism algebra + abductive
;;                  Monte Carlo sampler. GF(3) conservation: inject/init=+1,
;;                  project/terminal=-1, comp/pair=0. For agent-o-rama.
;;   sophia-mnemosyne.el — RDF knowledge graph for proof persistence
;;   causal-catcolab.el  — CatColab double theory UI
;;
;; Register new backends via `causal-proof-register-backend'.
;;
;; INSTALLATION
;;   (require 'causal-proof)
;;   (keymap-global-set "C-c p" #'causal-proof-tmenu)
;;
;; Or per-mode:
;;   (with-eval-after-load 'tuareg
;;     (keymap-set tuareg-mode-map "C-o" #'causal-proof-tmenu))

;;; Code:
(require 'transient)
(require 'causal-lib)
(require 'causal-proof-utils)
(require 'causal-proof-settings)
(require 'causal-catcolab nil t)
(require 'causal-xypic nil t)
(require 'causal-self-walker nil t)
(require 'sophia-mnemosyne nil t)

;;; ── Navigation ───────────────────────────────────────────────────────

(defun causal-proof-step-forward ()
  "Step forward one proof command / error."
  (interactive)
  (causal-proof--dispatch :step-fwd))

(defun causal-proof-step-backward ()
  "Step backward one proof command / error."
  (interactive)
  (causal-proof--dispatch :step-back))

(defun causal-proof-goto-point ()
  "Process up to point."
  (interactive)
  (causal-proof--dispatch :to-point))

(defun causal-proof-process-buffer ()
  "Process / typecheck entire buffer."
  (interactive)
  (causal-proof--dispatch :process))

(defun causal-proof-retract ()
  "Retract to beginning."
  (interactive)
  (causal-proof--dispatch :retract))

(defun causal-proof-goto-end-of-locked ()
  "Jump to end of locked / processed region."
  (interactive)
  (causal-proof--dispatch :locked-end))

;;; ── Goals & Context ──────────────────────────────────────────────────

(defun causal-proof-show-goals ()
  "Display goals / type at point."
  (interactive)
  (causal-proof--dispatch :goals))

(defun causal-proof-show-context ()
  "Display context / hypotheses / environment."
  (interactive)
  (causal-proof--dispatch :context))

;;; ── Holes ────────────────────────────────────────────────────────────

(defun causal-proof-next-hole ()
  "Jump to next typed hole / underscore / question mark."
  (interactive)
  (or (re-search-forward "[?_]\\|@hole\\|sorry\\|admit" nil t)
      (message "No more holes.")))

(defun causal-proof-prev-hole ()
  "Jump to previous typed hole."
  (interactive)
  (or (re-search-backward "[?_]\\|@hole\\|sorry\\|admit" nil t)
      (message "No earlier holes.")))

;;; ── Search ───────────────────────────────────────────────────────────

(defun causal-proof-search ()
  "Search for a lemma / definition / identifier."
  (interactive)
  (causal-proof--dispatch :search "Search not available for this backend."))

;;; ── Typecheck & Compile ──────────────────────────────────────────────

(defun causal-proof-typecheck ()
  "Typecheck current buffer (if backend supports it)."
  (interactive)
  (causal-proof--dispatch :typecheck "Typecheck not available."))

(defun causal-proof-compile ()
  "Compile / build (if backend supports it)."
  (interactive)
  (causal-proof--dispatch :compile "Compile not available."))

;;; ── Tactic Insertion ─────────────────────────────────────────────────

(defun causal-proof--insert-tactic (tactic)
  "Insert TACTIC at point followed by a space."
  (insert tactic " "))

(defun causal-proof-tactic-intro ()
  "Insert intro." (interactive) (causal-proof--insert-tactic "intro"))
(defun causal-proof-tactic-apply ()
  "Insert apply." (interactive) (causal-proof--insert-tactic "apply"))
(defun causal-proof-tactic-exact ()
  "Insert exact." (interactive) (causal-proof--insert-tactic "exact"))
(defun causal-proof-tactic-rewrite ()
  "Insert rewrite." (interactive) (causal-proof--insert-tactic "rewrite"))
(defun causal-proof-tactic-simp ()
  "Insert simp." (interactive) (causal-proof--insert-tactic "simp"))
(defun causal-proof-tactic-ring ()
  "Insert ring." (interactive) (causal-proof--insert-tactic "ring"))
(defun causal-proof-tactic-omega ()
  "Insert omega." (interactive) (causal-proof--insert-tactic "omega"))
(defun causal-proof-tactic-cases ()
  "Insert cases." (interactive) (causal-proof--insert-tactic "cases"))
(defun causal-proof-tactic-induction ()
  "Insert induction." (interactive) (causal-proof--insert-tactic "induction"))
(defun causal-proof-tactic-rfl ()
  "Insert rfl." (interactive) (causal-proof--insert-tactic "rfl"))
(defun causal-proof-tactic-destruct ()
  "Insert destruct." (interactive) (causal-proof--insert-tactic "destruct"))
(defun causal-proof-tactic-unfold ()
  "Insert unfold." (interactive) (causal-proof--insert-tactic "unfold"))

;;; ── Export / Integration ─────────────────────────────────────────────

(defun causal-proof-save-to-mnemosyne ()
  "Save current buffer to Mnemosyne knowledge graph."
  (interactive)
  (if (fboundp 'sophia-mnemosyne-save-buffer)
      (sophia-mnemosyne-save-buffer)
    (user-error "sophia-mnemosyne not loaded")))

(defun causal-proof-wire-dep (from to)
  "Wire a proof dependency FROM -> TO in Mnemosyne."
  (interactive "sFrom: \nsTo: ")
  (if (fboundp 'sophia-mnemosyne-create-wire)
      (sophia-mnemosyne-create-wire from to "proof:dependsOn")
    (user-error "sophia-mnemosyne not loaded")))

;;; ── Window Layout ────────────────────────────────────────────────────

(defun causal-proof-layout-three ()
  "Three-window layout."
  (interactive)
  (causal-proof--dispatch :layout))

(defun causal-proof-layout-two ()
  "Two-window layout (script | goals)."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (causal-proof--dispatch :goals)
  (other-window -1))

;;; ── Sub-menus ────────────────────────────────────────────────────────

(transient-define-prefix causal-proof-tactics-tmenu ()
  "Tactic insertion."
  ["Introduce & Eliminate"
   ("i" "intro"     causal-proof-tactic-intro)
   ("a" "apply"     causal-proof-tactic-apply)
   ("e" "exact"     causal-proof-tactic-exact)
   ("c" "cases"     causal-proof-tactic-cases)
   ("I" "induction" causal-proof-tactic-induction)
   ("d" "destruct"  causal-proof-tactic-destruct)
   ("u" "unfold"    causal-proof-tactic-unfold)]

  ["Simplify & Solve"
   ("s" "simp"    causal-proof-tactic-simp)
   ("r" "ring"    causal-proof-tactic-ring)
   ("o" "omega"   causal-proof-tactic-omega)
   ("=" "rfl"     causal-proof-tactic-rfl)
   ("w" "rewrite" causal-proof-tactic-rewrite)]

  [:class transient-row
   (causal-lib-quit-one)])

(transient-define-prefix causal-proof-holes-tmenu ()
  "Hole navigation."
  ["Holes"
   ("n" "Next hole" causal-proof-next-hole
    :description (lambda () (causal-proof-unicode-get :down-arrow))
    :transient t)
   ("p" "Prev hole" causal-proof-prev-hole
    :description (lambda () (causal-proof-unicode-get :up-arrow))
    :transient t)]

  [:class transient-row
   (causal-lib-quit-one)])

(transient-define-prefix causal-proof-window-tmenu ()
  "Window layout."
  ["Layout"
   ("3" "Three windows" causal-proof-layout-three)
   ("2" "Two windows"   causal-proof-layout-two)
   ("1" "Single window" delete-other-windows)]

  [:class transient-row
   (causal-lib-quit-one)])

;;; ── Main Menu ────────────────────────────────────────────────────────

;;;###autoload (autoload 'causal-proof-tmenu "causal-proof" nil t)

(transient-define-prefix causal-proof-tmenu ()
  "Causal Proof — unified proof assistant menu."

  [:description
   (lambda () (format "Proof [%s]" (causal-proof--backend-name)))]

  [["Navigation"
    ("n" "Step forward" causal-proof-step-forward
     :description (lambda () (causal-proof-unicode-get :step-forward))
     :transient t)
    ("p" "Step backward" causal-proof-step-backward
     :description (lambda () (causal-proof-unicode-get :step-backward))
     :transient t)
    ("RET" "To point" causal-proof-goto-point
     :description (lambda () (causal-proof-unicode-get :to-point)))
    ("b" "Process buffer" causal-proof-process-buffer)
    ("." "End of locked" causal-proof-goto-end-of-locked
     :description (lambda () (causal-proof-unicode-get :lock))
     :transient t)
    ("r" "Retract" causal-proof-retract
     :description (lambda () (causal-proof-unicode-get :retract)))]

   ["Goals & Holes"
    ("g" "Show goals" causal-proof-show-goals
     :description (lambda () (causal-proof-unicode-get :goal))
     :transient t)
    ("c" "Show context" causal-proof-show-context
     :description (lambda () (causal-proof-unicode-get :context))
     :transient t)
    ("h" "Holes>" causal-proof-holes-tmenu)
    ("t" "Tactics>" causal-proof-tactics-tmenu)]]

  [["Search & Build"
    ("s" "Search…" causal-proof-search)
    ("/" "I-search…" isearch-forward)
    ("i" "Imenu…" imenu)
    ("T" "Typecheck" causal-proof-typecheck
     :description (lambda () (causal-proof-unicode-get :typecheck)))
    ("B" "Compile" causal-proof-compile
     :description (lambda () (causal-proof-unicode-get :compile)))]

   ["Window"
    ("w" "Layout>" causal-proof-window-tmenu)]

   ["Diagrams & Export"
    ("x" "XY-pic 2-cells>" causal-xypic-tmenu
     :if (lambda () (featurep 'causal-xypic)))
    ("S" "Self-walk>" causal-self-walker-tmenu
     :if (lambda () (featurep 'causal-self-walker)))
    ("C" "CatColab>" causal-catcolab-tmenu
     :if (lambda () (featurep 'causal-catcolab)))
    ("O" "Proof > Olog" causal-catcolab-save-proof-as-olog
     :if (lambda () (featurep 'causal-catcolab)))
    ("M" "Save to Mnemosyne" causal-proof-save-to-mnemosyne
     :if (lambda () (featurep 'sophia-mnemosyne)))
    ("W" "Wire dep…" causal-proof-wire-dep
     :if (lambda () (featurep 'sophia-mnemosyne)))
    ("P" "Paperproof" causal-self-walker-paperproof-view
     :if (lambda () (featurep 'causal-self-walker-paperproof)))]]

  [:class transient-row
   (causal-lib-quit-one)
   ("," "Settings>" causal-proof-settings-tmenu)
   (causal-lib-quit-all)])

(provide 'causal-proof)
;;; causal-proof.el ends here
