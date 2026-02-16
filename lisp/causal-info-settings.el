;;; causal-info-settings.el --- Causal Info Settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Y. Choi

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
(require 'info)
(require 'causal-lib)
(require 'causal-info-variables)

(require 'causal-info-utils)

;;; Menus
(transient-define-prefix causal-info-settings-tmenu ()
  ["Customize Info"
   [("h" "Hide note references"
     causal-info--customize-info-hide-note-references)
    ("A" "Additional directories list"
     causal-info--customize-info-additional-directory-list)

    ("G" "Info Group"
     causal-info--customize-info-group)
    ]

   [("i" "I-search multiple nodes"
     causal-info--customize-info-isearch-search
     :description (lambda ()
                    (causal-lib-checkbox-label
                     Info-isearch-search
                     "I-search through multiple nodes")))
    ("c" "Prefer subnodes when scrolling"
     causal-info--customize-info-scroll-prefer-subnodes
     :description (lambda ()
                    (causal-lib-checkbox-label
                     Info-scroll-prefer-subnodes
                     "Prefer subnodes when scrolling")))]]

  [:class transient-row
   (causal-lib-customize-unicode)
   (causal-lib-customize-hide-navigation)]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-info-about :transient nil)
   (causal-lib-quit-all)])

;;; Functions
(defun causal-info--customize-info-isearch-search ()
  "If non-nil, isearch through multiple nodes.

Customize variable `Info-isearch-search'."
  (interactive)
  (customize-variable 'Info-isearch-search))

(defun causal-info--customize-info-additional-directory-list ()
  "List of additional directories to search for Info documentation files.

Customize variable `Info-additional-directory-list'.
These directories are searched after those in `Info-directory-list'."
  (interactive)
  (customize-variable 'Info-additional-directory-list))

(defun causal-info--customize-info-scroll-prefer-subnodes ()
  "If non-nil, \\<Info-mode-map>\\[Info-scroll-up] in a menu visits subnodes.

Customize variable `Info-scroll-prefer-subnodes'."
  (interactive)
  (customize-variable 'Info-scroll-prefer-subnodes))

(defun causal-info--customize-info-hide-note-references ()
  "If non-nil, hide the tag and section reference in *note and * menu items.

Customize variable `Info-hide-note-references'."
  (interactive)
  (customize-variable 'Info-hide-note-references))

(defun causal-info--customize-info-group ()
  "Call the Info customization group.

Sadly, Info variables are not grouped so this command is really of
limited usefulness."
  (interactive)
  (customize-group "info"))

(defun causal-info-about-info ()
  "Causal Info is an opinionated porcelain for Emacs Info.

Learn more about using Causal Info at our discussion group on GitHub.
Any questions or comments about Causal should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal Info, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal Info was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Causal Info.

Always choose love."
  (ignore))

(defun causal-info-about ()
  "About information for Causal Info."
  (interactive)
  (describe-function #'causal-info-about-info))

(provide 'causal-info-settings)
;;; causal-info-settings.el ends here
