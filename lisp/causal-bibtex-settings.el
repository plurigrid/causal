;;; causal-bibtex-settings.el --- Causal BibTeX Settings -*- lexical-binding: t; -*-

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
(require 'causal-lib)

(transient-define-prefix causal-bibtex-settings-tmenu ()
  "Causal BibTeX settings menu."
  ["Causal BibTeX Settings"
   ["Settings"
    ("d" "Dialect" causal-bibtex--customize-dialect
     :description (lambda ()
                    (if bibtex-dialect
                        (format "Dialect (%s)" bibtex-dialect)
                      "Dialect")))
    ("G" "BibTeX Group" causal-bibtex--customize-group)]

   ["Files"
    ("p" "Path" causal-bibtex--customize-file-path
      :description (lambda ()
                    (if bibtex-file-path
                        (format "Path (%s)" bibtex-file-path)
                      "Path")))
    ("f" "Files" causal-bibtex--customize-files)]

   ["Search"
    ("g" "Entry Globally" causal-bibtex--customize-search-entry-globally
     :description (lambda () (causal-lib-checkbox-label bibtex-search-entry-globally
                                                   "Entry Globally")))]

   ["Hooks"
    ("C" "Clean" causal-bibtex--customize-clean-entry-hook)
    ("A" "Add" causal-bibtex--customize-add-entry-hook)]]

  ["Misc"
    :class transient-row
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-bibtex-about :transient nil)
   (causal-lib-quit-all)])

(defun causal-bibtex-about-bibtex ()
  "Causal BibTeX is a Transient menu for BibTeX.

Learn more about using Causal BibTeX at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal BibTeX, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal BibTeX was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Causal BibTeX.

Always choose love."
  (ignore))

(defun causal-bibtex-about ()
  "About information for Causal BibTeX."
  (interactive)
  (describe-function #'causal-bibtex-about-bibtex))


;;; Customize Functions
(defun causal-bibtex--customize-group ()
  "Customize BibTeX group."
  (interactive)
  (customize-group "bibtex"))

(defun causal-bibtex--customize-dialect ()
  "Set BibTeX dialect.

This customizes the variable `bibtex-dialect'."
  (interactive)
  (customize-variable 'bibtex-dialect))

(defun causal-bibtex--customize-files ()
  "Set BibTeX files.

This customizes the variable `bibtex-files'."
  (interactive)
  (customize-variable 'bibtex-files))

(defun causal-bibtex--customize-file-path ()
  "Set BibTeX file path.

This customizes the variable `bibtex-file-path'."
  (interactive)
  (customize-variable 'bibtex-file-path))

(defun causal-bibtex--customize-search-entry-globally ()
  "Set BibTeX search to be global.

This customizes the variable `bibtex-search-entry-globally'."
  (interactive)
  (customize-variable 'bibtex-search-entry-globally))

(defun causal-bibtex--customize-clean-entry-hook ()
  "Set hook for cleaning a BibTeX entry.

This customizes the variable `bibtex-clean-entry-hook'."
  (interactive)
  (customize-variable 'bibtex-clean-entry-hook))

(defun causal-bibtex--customize-add-entry-hook ()
  "Set hook for adding a BibTeX entry.

This customizes the variable `bibtex-add-entry-hook'."
  (interactive)
  (customize-variable 'bibtex-add-entry-hook))


(provide 'causal-bibtex-settings)
;;; causal-bibtex-settings.el ends here
