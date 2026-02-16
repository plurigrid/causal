;;; causal-html-settings.el --- Causal HTML Settings -*- lexical-binding: t; -*-

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
(require 'sgml-mode)
(require 'causal-lib)

(defvar html-ts-mode-indent-offset)

(transient-define-prefix causal-html-settings-tmenu ()
  "Causal HTML settings menu."
  ["Causal HTML: Settings"
   ["Offsets"
    ("b" "Basic" causal-html--customize-sgml-basic-offset
     :description (lambda () (format "Basic (%d)" sgml-basic-offset)))
    ("t" "Tree-sitter Basic" causal-html--customize-html-ts-mode-indent-offset
     :if (lambda () (derived-mode-p 'html-ts-mode))
     :description (lambda () (format "Tree-sitter Basic (%d)"
                                html-ts-mode-indent-offset)))
    ("A" "Attribute" causal-html--customize-sgml-attribute-offset
     :description (lambda () (format "Attribute (%d)" sgml-attribute-offset)))]

   ["Group"
    ("G" "SGML Group" causal-html--customize-group-sgml)]]

  ["General"
   :class transient-row
   (causal-lib-customize-unicode)
   (causal-lib-customize-hide-navigation)]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-html-about :transient nil)
   (causal-lib-quit-all)])

(defun causal-html--customize-group-sgml ()
  "Customize SGML group."
  (interactive)
  (customize-group "sgml"))

(defun causal-html--customize-sgml-basic-offset ()
  "Customize SGML `sgml-basic-offset'."
  (interactive)
  (customize-variable 'sgml-basic-offset))

(defun causal-html--customize-sgml-attribute-offset ()
  "Customize SGML `sgml-attribute-offset'."
  (interactive)
  (customize-variable 'sgml-attribute-offset))

(defun causal-html--customize-html-ts-mode-indent-offset ()
  "Customize SGML `html-ts-mode-indent-offset'."
  (interactive)
  (customize-variable 'html-ts-mode-indent-offset))

(defun causal-html-about-html ()
  "Causal HTML is a Transient menu for HTML mode.

Learn more about using Causal HTML at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal HTML, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal HTML was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Causal HTML.

Always choose love."
  (ignore))

(defun causal-html-about ()
  "About information for Causal HTML."
  (interactive)
  (describe-function #'causal-html-about-html))

(provide 'causal-html-settings)
;;; causal-html-settings.el ends here
