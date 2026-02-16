;;; causal-csv-settings.el --- Causal CSV Settings -*- lexical-binding: t; -*-

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
(require 'csv-mode)
(require 'causal-lib)

(transient-define-prefix causal-csv-settings-tmenu ()
  "Causal csv settings menu."
  ["Causal csv: Settings"
   ["Customize"
    ("A" "Align Style" causal-csv--customize-align-style
     :description (lambda () (format "Align Style (%s)"
                                (capitalize (symbol-name csv-align-style)))))
    ("s" "Separators" causal-csv--customize-separators)
    ("i" "Invisibility Default" causal-csv--customize-invisibility-default
     :description (lambda () (causal-lib-checkbox-label csv-invisibility-default
                                                   "Invisibility Default")))]

   [""
    ("G" "CSV Group" causal-csv--customize-group)
    ("h" "Header Lines" causal-csv--customize-header-lines
     :description (lambda () (format "Header Lines (%d)" csv-header-lines)))
    ("c" "Comment Start Default" causal-csv--customize-comment-start-default
     :description (lambda () (format
                         "Comment Start Default (%s)"
                         csv-comment-start-default)))
    ("f" "Field Quotes" causal-csv--customize-field-quotes
     :description (lambda () (format
                         "Field Quotes (%s)"
                         (string-join csv-field-quotes))))]

   ["Width"
    ("w" "Min" causal-csv--customize-align-min-width
     :description (lambda () (format "Min (%d)" csv-align-min-width)))
    ("W" "Max" causal-csv--customize-align-max-width
     :description (lambda () (format "Max (%d)" csv-align-max-width)))]]

  [:class transient-row
   (causal-lib-customize-unicode)
   (causal-lib-customize-hide-navigation)]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-csv-about)
   (causal-lib-quit-all)])


;; -------------------------------------------------------------------
;; Functions

(defun causal-csv--customize-group ()
  "Customize csv group."
  (interactive)
  (customize-group "CSV"))

(defun causal-csv--customize-align-style ()
  "Customize `csv-align-style'."
  (interactive)
  (customize-variable 'csv-align-style))

(defun causal-csv--customize-separators ()
  "Customize `csv-separators'."
  (interactive)
  (customize-variable 'csv-separators))

(defun causal-csv--customize-field-quotes ()
  "Customize `csv-field-quotes'."
  (interactive)
  (customize-variable 'csv-field-quotes))

(defun causal-csv--customize-align-max-width ()
  "Customize `csv-align-max-width'."
  (interactive)
  (customize-variable 'csv-align-max-width))

(defun causal-csv--customize-align-min-width ()
  "Customize `csv-align-min-width'."
  (interactive)
  (customize-variable 'csv-align-min-width))

(defun causal-csv--customize-invisibility-default ()
  "Customize `csv-invisibility-default'."
  (interactive)
  (customize-variable 'csv-invisibility-default))

(defun causal-csv--customize-comment-start-default ()
  "Customize `csv-comment-start-default'."
  (interactive)
  (customize-variable 'csv-comment-start-default))

(defun causal-csv--customize-header-lines ()
  "Customize `csv-comment-header-lines'."
  (interactive)
  (customize-variable 'csv-header-lines))

(defun causal-csv-about-csv ()
  "Causal csv is a Transient menu for csv pages.

Learn more about using Causal csv at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal csv, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal csv was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Causal csv.

Always choose love."
  (ignore))

(defun causal-csv-about ()
  "About information for Causal csv."
  (interactive)
  (describe-function #'causal-csv-about-csv))

(provide 'causal-csv-settings)
;;; causal-csv-settings.el ends here
