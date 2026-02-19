;;; causal-ucm-settings.el --- Causal UCM Settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

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

;; Settings and about information for Causal UCM, a Transient-based
;; porcelain for the Unison Codebase Manager (UCM).

;;; Code:
(require 'causal-lib)

(defgroup causal-ucm nil
  "Settings for Causal UCM."
  :group 'causal)

(defcustom causal-ucm-executable "ucm"
  "Path or name of the UCM executable."
  :type 'string
  :group 'causal-ucm)

(transient-define-prefix causal-ucm-settings-tmenu ()
  "Causal UCM settings menu."
  ["Causal UCM: Settings"
   [("U" "UCM Group" causal-ucm--customize-group)
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-ucm-about :transient nil)
   (causal-lib-quit-all)])

(defun causal-ucm--customize-group ()
  "Customize UCM group."
  (interactive)
  (customize-group "causal-ucm"))

(defun causal-ucm-about-ucm ()
  "Causal UCM is a Transient menu for the Unison Codebase Manager.

UCM is the command-line tool for the Unison programming language,
managing content-addressed codebases where definitions are identified
by their structure rather than by name.

Learn more about Unison at URL `https://www.unison-lang.org/'
Learn more about Causal at URL `https://github.com/plurigrid/causal'

If you find a bug or have an enhancement request, please file an issue.
URL `https://github.com/plurigrid/causal/issues'

Always choose love."
  (ignore))

(defun causal-ucm-about ()
  "About information for Causal UCM."
  (interactive)
  (describe-function #'causal-ucm-about-ucm))

(provide 'causal-ucm-settings)
;;; causal-ucm-settings.el ends here
