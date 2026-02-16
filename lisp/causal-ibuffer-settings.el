;;; causal-ibuffer-settings.el --- Causal IBuffer Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools

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
(require 'ibuffer)
(require 'causal-lib)


(transient-define-prefix causal-ibuffer-settings-tmenu ()
  "Causal IBuffer settings menu."
  ["IBuffer: Settings"
   ["Customize"
   ("f" "Saved Filters" causal-ibuffer--customize-ibuffer-saved-filters)
   ("g" "Saved Filter Groups" causal-ibuffer--customize-ibuffer-saved-filter-groups)
   ("G" "IBuffer Group" causal-ibuffer--customize-group)
   (causal-lib-customize-unicode)
   (causal-lib-customize-hide-navigation)]]

  [:class transient-row
          ("a" "About" causal-ibuffer-about :transient nil)

          (causal-lib-quit-one)
          (causal-lib-quit-all)])

(defun causal-ibuffer--customize-ibuffer-saved-filters ()
  "Customize `ibuffer-saved-filters'."
  (interactive)
  (customize-variable 'ibuffer-saved-filters))

(defun causal-ibuffer--customize-ibuffer-saved-filter-groups ()
  "Customize `ibuffer-saved-filter-groups'."
  (interactive)
  (customize-variable 'ibuffer-saved-filter-groups))

(defun causal-ibuffer--customize-group ()
  "Customize IBuffer group."
  (interactive)
  (customize-group "ibuffer"))

(defun causal-ibuffer-about-ibuffer ()
  "Causal IBuffer is a Transient menu for IBuffer.

Learn more about using Causal IBuffer at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal IBuffer, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal IBuffer was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Causal IBuffer.

Always choose love."
  (ignore))

(defun causal-ibuffer-about ()
  "About information for Causal IBuffer."
  (interactive)
  (describe-function #'causal-ibuffer-about-ibuffer))

(provide 'causal-ibuffer-settings)
;;; causal-ibuffer-settings.el ends here
