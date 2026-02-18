;;; causal-proofreader-utils.el --- Causal ProofReader Utils  -*- lexical-binding: t; -*-

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

;; Utility functions for Causal ProofReader, a Transient-based porcelain
;; for proof assistants (Proof General, Lean 4, Narya).

;;; Code:

(require 'transient)
(require 'causal-lib)

;;; Unicode Database

(defconst causal-proofreader-unicode-db
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
    (:bridge           . '("≡" "Bridge")))
  "Unicode symbol DB for ProofReader Transient menus.")

(defun causal-proofreader-unicode-get (key)
  "Lookup Unicode symbol for KEY in `causal-proofreader-unicode-db'.

If `causal-lib-use-unicode' is non-nil, the Unicode symbol is
returned, otherwise a plain ASCII-range string."
  (causal-lib-unicode-db-get key causal-proofreader-unicode-db))

;;; Backend Detection

(defun causal-proofreader--proof-general-p ()
  "Return non-nil if Proof General is loaded and active."
  (and (featurep 'proof-site)
       (bound-and-true-p proof-mode)))

(defun causal-proofreader--lean-mode-p ()
  "Return non-nil if lean4-mode is active."
  (or (derived-mode-p 'lean4-mode)
      (derived-mode-p 'lean-mode)))

(defun causal-proofreader--narya-mode-p ()
  "Return non-nil if narya-mode is active."
  (derived-mode-p 'narya-mode))

(defun causal-proofreader--backend ()
  "Detect which proof assistant backend is active.
Returns one of: `proof-general', `lean', `narya', or nil."
  (cond
   ((causal-proofreader--narya-mode-p) 'narya)
   ((causal-proofreader--lean-mode-p) 'lean)
   ((causal-proofreader--proof-general-p) 'proof-general)
   (t nil)))

(defun causal-proofreader--backend-name ()
  "Return human-readable name of active proof backend."
  (pcase (causal-proofreader--backend)
    ('narya "Narya")
    ('lean "Lean")
    ('proof-general "Proof General")
    (_ "No backend")))

;;; Dispatch Helpers

(defun causal-proofreader--dispatch (pg-fn lean-fn narya-fn &optional fallback-msg)
  "Dispatch to appropriate function based on active backend.
PG-FN for Proof General, LEAN-FN for Lean, NARYA-FN for Narya.
FALLBACK-MSG shown if no backend detected."
  (let ((backend (causal-proofreader--backend)))
    (pcase backend
      ('proof-general (call-interactively pg-fn))
      ('lean (call-interactively lean-fn))
      ('narya (call-interactively narya-fn))
      (_ (message (or fallback-msg "No proof assistant detected in current buffer."))))))

(provide 'causal-proofreader-utils)
;;; causal-proofreader-utils.el ends here
