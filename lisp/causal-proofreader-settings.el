;;; causal-proofreader-settings.el --- Causal ProofReader Settings  -*- lexical-binding: t; -*-

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

;;

;;; Code:
(require 'transient)
(require 'causal-lib)
(require 'causal-proofreader-utils)

;;; Customization Variables

(defgroup causal-proofreader nil
  "Causal ProofReader settings."
  :group 'tools
  :prefix "causal-proofreader-")

(defcustom causal-proofreader-window-policy 'hybrid
  "Window layout policy for proof assistant buffers.
`hybrid' uses proof-three-window-mode-policy hybrid.
`three' always uses three windows (script, goals, response).
`two' uses two windows (script and goals)."
  :type '(choice (const :tag "Hybrid" hybrid)
                 (const :tag "Three windows" three)
                 (const :tag "Two windows" two))
  :group 'causal-proofreader)

(defcustom causal-proofreader-auto-detect-backend t
  "If non-nil, auto-detect the proof assistant from the buffer mode."
  :type 'boolean
  :group 'causal-proofreader)

;;; Settings Menu

(transient-define-prefix causal-proofreader-settings-tmenu ()
  "Causal ProofReader settings."
  ["Customize ProofReader"
   [("b" "Backend auto-detect"
     causal-proofreader--customize-auto-detect
     :description (lambda ()
                    (causal-lib-checkbox-label
                     causal-proofreader-auto-detect-backend
                     "Auto-detect backend")))
    ("w" "Window layout policy"
     causal-proofreader--customize-window-policy)
    ("G" "ProofReader Group"
     causal-proofreader--customize-group)]]

  [:class transient-row
   (causal-lib-customize-unicode)
   (causal-lib-customize-hide-navigation)]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-proofreader-about :transient nil)
   (causal-lib-quit-all)])

;;; Customize Functions

(defun causal-proofreader--customize-auto-detect ()
  "Customize `causal-proofreader-auto-detect-backend'."
  (interactive)
  (customize-variable 'causal-proofreader-auto-detect-backend))

(defun causal-proofreader--customize-window-policy ()
  "Customize `causal-proofreader-window-policy'."
  (interactive)
  (customize-variable 'causal-proofreader-window-policy))

(defun causal-proofreader--customize-group ()
  "Open the ProofReader customization group."
  (interactive)
  (customize-group "causal-proofreader"))

;;; About

(defun causal-proofreader-about-info ()
  "Causal ProofReader is a Transient porcelain for proof assistants.

Supported backends: Proof General (Coq, Narya), Lean 4, Narya.

Causal ProofReader provides discoverable, keyboard-driven menus
for stepping through proofs, managing goals and holes, dispatching
tactics, and searching for theorems.

Source: URL `https://github.com/plurigrid/causal'

Causal ProofReader was crafted by the Plurigrid community."
  (ignore))

(defun causal-proofreader-about ()
  "About information for Causal ProofReader."
  (interactive)
  (describe-function #'causal-proofreader-about-info))

(provide 'causal-proofreader-settings)
;;; causal-proofreader-settings.el ends here
