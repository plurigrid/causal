;;; causal-proof-utils.el --- Unified proof assistant utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Extensible backend registry and dispatch for causal-proof.
;; Each backend is a plist with detection predicate and per-operation
;; function slots.  New backends register via `causal-proof-register-backend'.
;;
;; ARCHITECTURE (for Amp and other agents reading this file):
;;
;;   causal-proof-utils.el  ← YOU ARE HERE: backend registry + unicode DB
;;   causal-proof.el        ← Transient UI: navigation, tactics, holes, export
;;   causal-proof-settings.el ← customization, backend listing, about
;;
;; Built-in backends (5):
;;   Narya        — standalone narya-mode (dependent types)
;;   Proof General — Coq, Isabelle, ... (via proof-site)
;;   Lean         — Lean 4 (lean4-mode)
;;   OCaml        — OxCaml / tuareg-mode + Merlin
;;   DoubleTT     — CatColab double type theory (.dtt files)
;;
;; Each backend plist has these keys:
;;   :name       String   — human-readable name
;;   :detect     Function — returns non-nil when this backend is active
;;   :step-fwd   Function — step forward one proof command
;;   :step-back  Function — step backward
;;   :to-point   Function — process to point
;;   :process    Function — process entire buffer
;;   :retract    Function — retract to start
;;   :locked-end Function — jump to end of locked region
;;   :goals      Function — show goals / type at point
;;   :context    Function — show context / hypotheses
;;   :search     Function — search lemma/definition
;;   :layout     Function — arrange windows
;;   :typecheck  Function — typecheck buffer (optional)
;;   :compile    Function — compile/build (optional)
;;
;; Related: geb-mc.lisp (~/i/geb/src/) provides morphism-level
;; abductive Monte Carlo for agent-o-rama, also using GF(3) trits.
;; sophia-mnemosyne.el provides RDF knowledge graph persistence.

;;; Code:

(require 'transient)
(require 'causal-lib)

;;; Unicode Database

(defconst causal-proof-unicode-db
  '((:step-forward     . '("⏩" ">>"))
    (:step-backward    . '("⏪" "<<"))
    (:to-point         . '("⏯" "=>"))
    (:retract          . '("⏏" "Retract"))
    (:goal             . '("⊢" "Goal"))
    (:context          . '("Γ" "Ctx"))
    (:hole             . '("◯" "?"))
    (:tactic           . '("τ" "Tactic"))
    (:check            . '("✓" "OK"))
    (:lock             . '("█" "Locked"))
    (:search           . '("🔍" "Search"))
    (:up-arrow         . '("↑" "Prev"))
    (:down-arrow       . '("↓" "Next"))
    (:window           . '("⊞" "Layout"))
    (:bridge           . '("≡" "Bridge"))
    (:morphism         . '("→" "->"))
    (:object           . '("●" "Ob"))
    (:theory           . '("𝕋" "Th"))
    (:elaborate        . '("⊨" "Elab"))
    (:typecheck        . '("⊢" "TC"))
    (:compile          . '("⚙" "Build")))
  "Unicode symbol DB for Causal Proof menus.")

(defun causal-proof-unicode-get (key)
  "Lookup Unicode symbol for KEY in `causal-proof-unicode-db'."
  (causal-lib-unicode-db-get key causal-proof-unicode-db))

;;; ── Backend Registry ─────────────────────────────────────────────────
;;
;; Each backend is a plist:
;;   :name       "Human Name"
;;   :detect     (lambda () BOOL)
;;   :step-fwd   #'fn   — step one command forward
;;   :step-back  #'fn   — step one command backward
;;   :to-point   #'fn   — process to point
;;   :process    #'fn   — process entire buffer
;;   :retract    #'fn   — retract to start
;;   :locked-end #'fn   — jump to end of locked
;;   :goals      #'fn   — show goals
;;   :context    #'fn   — show context
;;   :search     #'fn   — search lemma/definition
;;   :layout     #'fn   — arrange windows
;;   :typecheck  #'fn   — typecheck buffer (optional)
;;   :compile    #'fn   — compile/build (optional)
;;
;; Backends are tried in order; first whose :detect returns non-nil wins.

(defvar causal-proof-backends nil
  "Ordered list of registered proof backends (plists).")

(defun causal-proof-register-backend (backend)
  "Register BACKEND (a plist) at the end of `causal-proof-backends'.
If a backend with the same :name exists, replace it."
  (let ((name (plist-get backend :name)))
    (setq causal-proof-backends
          (cl-remove-if (lambda (b) (string= (plist-get b :name) name))
                        causal-proof-backends))
    (setq causal-proof-backends
          (append causal-proof-backends (list backend)))))

(defun causal-proof--active-backend ()
  "Return the first backend whose :detect returns non-nil, or nil."
  (cl-find-if (lambda (b) (funcall (plist-get b :detect)))
              causal-proof-backends))

(defun causal-proof--backend-name ()
  "Human-readable name of the active backend."
  (let ((b (causal-proof--active-backend)))
    (if b (plist-get b :name) "No backend")))

(defun causal-proof--dispatch (op &optional fallback-msg)
  "Call operation OP (a keyword like :step-fwd) on the active backend.
If no backend, show FALLBACK-MSG."
  (let ((b (causal-proof--active-backend)))
    (if b
        (let ((fn (plist-get b op)))
          (if fn
              (call-interactively fn)
            (message "%s: operation %s not supported" (plist-get b :name) op)))
      (message (or fallback-msg "No proof assistant detected.")))))

;;; ── Built-in Backend: Proof General (Coq, Narya, ...) ───────────────

(causal-proof-register-backend
 (list :name "Narya"
       :detect (lambda () (derived-mode-p 'narya-mode))
       :step-fwd  #'proof-assert-next-command-interactive
       :step-back #'proof-undo-last-successful-command
       :to-point  #'proof-goto-point
       :process   #'proof-process-buffer
       :retract   #'proof-retract-buffer
       :locked-end #'proof-goto-end-of-locked
       :goals     #'proof-display-some-buffers
       :context   #'proof-display-some-buffers
       :search    #'proof-find-theorems
       :layout    #'proof-layout-windows))

(causal-proof-register-backend
 (list :name "Proof General"
       :detect (lambda () (and (featurep 'proof-site)
                               (bound-and-true-p proof-mode)))
       :step-fwd  #'proof-assert-next-command-interactive
       :step-back #'proof-undo-last-successful-command
       :to-point  #'proof-goto-point
       :process   #'proof-process-buffer
       :retract   #'proof-retract-buffer
       :locked-end #'proof-goto-end-of-locked
       :goals     #'proof-display-some-buffers
       :context   #'proof-display-some-buffers
       :search    #'coq-SearchAbout
       :layout    #'proof-layout-windows))

;;; ── Built-in Backend: Lean 4 ────────────────────────────────────────

(causal-proof-register-backend
 (list :name "Lean"
       :detect (lambda () (or (derived-mode-p 'lean4-mode)
                              (derived-mode-p 'lean-mode)))
       :step-fwd  #'lean4-execute
       :step-back #'lean4-undo
       :to-point  #'lean4-refresh-file-dependencies
       :process   #'lean4-refresh-file-dependencies
       :retract   #'lean4-undo
       :locked-end #'beginning-of-buffer
       :goals     #'lean4-show-goal
       :context   #'lean4-show-goal
       :search    #'lean4-find-definition))

;;; ── Built-in Backend: OxCaml / OCaml ────────────────────────────────

(causal-proof-register-backend
 (list :name "OCaml"
       :detect (lambda () (or (derived-mode-p 'tuareg-mode)
                              (derived-mode-p 'caml-mode)))
       :step-fwd  #'merlin-error-next
       :step-back #'merlin-error-prev
       :to-point  #'merlin-error-check
       :process   #'compile
       :goals     #'merlin-type-enclosing
       :context   #'merlin-type-enclosing
       :search    #'merlin-locate
       :typecheck #'merlin-error-check
       :compile   #'compile))

;;; ── Built-in Backend: DoubleTT (CatColab) ───────────────────────────

(causal-proof-register-backend
 (list :name "DoubleTT"
       :detect (lambda () (or (derived-mode-p 'doublett-mode)
                              (and buffer-file-name
                                   (string-match-p "\\.dtt\\'" buffer-file-name))))
       :step-fwd  #'proof-assert-next-command-interactive
       :step-back #'proof-undo-last-successful-command
       :to-point  #'proof-goto-point
       :process   #'compile
       :goals     #'proof-display-some-buffers
       :context   #'proof-display-some-buffers
       :search    #'imenu))

(provide 'causal-proof-utils)
;;; causal-proof-utils.el ends here
