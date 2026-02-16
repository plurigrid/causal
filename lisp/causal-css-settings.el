;;; causal-css-settings.el --- Causal CSS Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charles Y. Choi

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
(require 'css-mode)
(require 'causal-lib)

(transient-define-prefix causal-css-settings-tmenu ()
  "Causal CSS settings menu."
  ["Causal CSS: Settings"
   ["Customize"
    ("o" "Indent Offset" causal-css--customize-indent-offset
     :description (lambda () (format "Indent Offset (%d)" css-indent-offset)))]

   ["Group"
    ("G" "CSS Group" causal-css--customize-group)]]

  ["General"
   :class transient-row
   (causal-lib-customize-unicode)
   (causal-lib-customize-hide-navigation)]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-css-about)
   (causal-lib-quit-all)])

(defun causal-css--customize-indent-offset ()
  "Customize CSS `css-indent-offset'."
  (interactive)
  (customize-variable 'css-indent-offset))

(defun causal-css--customize-group ()
  "Customize CSS group."
  (interactive)
  (customize-group "css"))

(defun causal-css-about-css ()
  "Causal CSS is a Transient menu for CSS mode.

Learn more about using Causal CSS at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal CSS, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal CSS was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Causal CSS.

Always choose love."
  (ignore))

(defun causal-css-about ()
  "About information for Causal CSS."
  (interactive)
  (describe-function #'causal-css-about-css))

(provide 'causal-css-settings)
;;; causal-css-settings.el ends here
