;;; causal-ediff-settings.el --- Causal Ediff Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charles Y. Choi

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
(require 'ediff)
(require 'causal-lib)

(transient-define-prefix causal-ediff-settings-tmenu ()
  "Causal Ediff settings menu."
  ["Causal Ediff: Settings"
   [("k" "Keep Variants"
     causal-ediff-customize-ediff-keep-variants
     :description (lambda () (causal-lib-checkbox-label ediff-keep-variants
                                                   "Keep Variants")))

    ("w" "Window Setup Function"
     causal-ediff-customize-ediff-window-setup-function)

    ("s" "Split Window Function"
     causal-ediff-customize-ediff-split-window-function)]

   [("G" "Ediff Group" causal-ediff-customize-group)
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-ediff-about :transient nil)
   (causal-lib-quit-all)])

(defun causal-ediff-customize-group ()
  "Customize Ediff group."
  (interactive)
  (customize-group "ediff"))

(defun causal-ediff-customize-ediff-keep-variants ()
  "Customize `ediff-keep-variants'."
  (interactive)
  (customize-variable 'ediff-keep-variants))

(defun causal-ediff-customize-ediff-window-setup-function ()
  "Customize `ediff-window-setup-function'."
  (interactive)
  (customize-variable 'ediff-window-setup-function))

(defun causal-ediff-customize-ediff-split-window-function ()
  "Customize `ediff-split-window-function'."
  (interactive)
  (customize-variable 'ediff-split-window-function))

(defun causal-ediff-about-ediff ()
  "Causal Ediff is a Transient menu for Ediff.

Learn more about using Causal Ediff at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal Ediff, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal Ediff was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Causal Ediff.

Always choose love."
  (ignore))

(defun causal-ediff-about ()
  "About information for Causal Ediff."
  (interactive)
  (describe-function #'causal-ediff-about-ediff))

(provide 'causal-ediff-settings)
;;; causal-ediff-settings.el ends here
