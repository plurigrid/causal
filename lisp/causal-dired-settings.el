;;; causal-dired-settings.el --- Causal Dired Settings  -*- lexical-binding: t; -*-

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
(require 'dired)
(require 'dired-aux)
(require 'wdired)
(require 'causal-lib)
(require 'causal-dired-utils)
(require 'causal-dired-variables)


;;; Menus
(transient-define-prefix causal-dired-settings-tmenu ()
  ["Dired Settings"
   ["Customize"
    ("r" "Revert Policy"
     causal-dired--customize-dired-auto-revert-buffer
     :transient nil)
    ("t" "Target Directory"
     causal-dired--customize-dired-dwim-target
     :transient nil)
    ("T" "Use System Trash Can"
     causal-dired--customize-delete-by-moving-to-trash
     :description (lambda ()
                   (causal-lib-checkbox-label
                    delete-by-moving-to-trash
                    "Use System Trash Can"))
     :transient nil)
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)
    ("R" "Rename via VC"
     causal-dired--customize-dired-vc-rename-file
     :description (lambda ()
                   (causal-lib-checkbox-label
                    dired-vc-rename-file
                    "Rename via VC"))
     :transient nil)

    ("v" "Visit Truename"
     causal-dired--customize-find-file-visit-truename
     :description (lambda ()
                    (causal-lib-checkbox-label
                     find-file-visit-truename
                     "Visit Truename")))]

   ["GNU ‘ls’"
    ("l" "Use GNU ‘ls’ with “--dired”"
     causal-dired--customize-dired-use-ls-dired
     :transient nil)
    ("s" "Initial Listing Switches"
     causal-dired--customize-dired-listing-switches
     :transient nil)
    ("c" "Initial Listing Switches for “Sort By” Menu"
     causal-dired--customize-causal-dired-listing-switches
     :transient nil)]]

  [["wdired"
    ("p" "Allow Changing Permissions"
     causal-dired--customize-wdired-allow-to-change-permissions
     :description (lambda ()
                   (causal-lib-checkbox-label
                    wdired-allow-to-change-permissions
                    "Allow Changing Permissions"))
     :transient nil)

    ("L" "Allow Redirecting Links"
     causal-dired--customize-wdired-allow-to-redirect-links
     :description (lambda ()
                   (causal-lib-checkbox-label
                    wdired-allow-to-redirect-links
                    "Allow Redirecting Links"))
     :transient nil)]

   ["Dired"
    ("d" "Dired Group"
     causal-dired--customize-dired-group
     :transient nil)]]

  [:class transient-row
          (causal-lib-quit-one)
          ("a" "About" causal-dired-about :transient nil)

          (causal-lib-quit-all)])

;;; Functions

(defun causal-dired--customize-dired-use-ls-dired ()
  "Customize if “--dired” switch is passed to ‘ls’.

Customize the variable `dired-use-ls-dired'.  If
`dired-use-ls-dired' is non-nil, then pass the “--dired” option
to ‘ls’.

NOTE: If `dired-use-ls-dired' is t, Causal Dired presumes that
GNU ‘ls’ is installed."
  (interactive)
  (customize-variable 'dired-use-ls-dired))

(defun causal-dired--customize-dired-auto-revert-buffer ()
  "Customize Dired revert buffer policy.

Customize the variable `dired-auto-revert-buffer' which
determines if Dired buffers are automatically reverted on
revisiting their directory."
  (interactive)
  (customize-variable 'dired-auto-revert-buffer))

(defun causal-dired--customize-dired-dwim-target ()
  "Customize `dired-dwim-target'.

If non-nil, Dired tries to guess a default target directory."
  (interactive)
  (customize-variable 'dired-dwim-target))

(defun causal-dired--customize-dired-listing-switches ()
  "Customize `dired-listing-switches'.

Switches passed to ‘ls’ for Dired.  MUST contain the ‘l’ option."
  (interactive)
  (customize-variable 'dired-listing-switches))

(defun causal-dired--customize-dired-vc-rename-file ()
    "Customize `dired-vc-rename-file'.

This variable configures whether Dired should register file
renaming in underlying vc system."
  (interactive)
  (customize-variable 'dired-vc-rename-file))

(defun causal-dired--customize-causal-dired-listing-switches ()
  "Customize `causal-dired-listing-switches'.

Customizes switches that initialize the Causal Dired “Sort By”
menu (`causal-dired-sort-by-tmenu').  Note that this variable is
unused if the user saves the state of this Transient menu.

To avoid unforeseen dependencies, this variable is independent
from the value of `dired-listing-switches'.  That said, it is
recommended to set both `dired-listing-switches' and
`causal-dired-listing-switches' to be consistent with each other."
  (interactive)
  (customize-variable 'causal-dired-listing-switches))

(defun causal-dired--customize-delete-by-moving-to-trash ()
  "Customize `delete-by-moving-to-trash'.

Customize the variable `delete-by-moving-to-trash'.  Specifies
whether to use the system’s trash can."
  (interactive)
  (customize-variable 'delete-by-moving-to-trash))

(defun causal-dired--customize-wdired-allow-to-change-permissions ()
  "Customize `wdired-allow-to-change-permissions'.

Customize the variable `wdired-allow-to-change-permissions'.
If non-nil, the permissions bits of the files are editable."
  (interactive)
  (customize-variable 'wdired-allow-to-change-permissions))

(defun causal-dired--customize-wdired-allow-to-redirect-links ()
  "Customize `wdired-allow-to-redirect-links'.

Customize the variable `wdired-allow-to-redirect-links'.
If non-nil, the target of the symbolic links are editable."
  (interactive)
  (customize-variable 'wdired-allow-to-redirect-links))

(defun causal-dired--customize-find-file-visit-truename ()
  "Customize `find-file-visit-truename'.

Customize the variable `find-file-visit-truename'. If non-nil,
visiting a file uses its truename as the visited-file name. This
effectively follows a symlink to its actual location."
  (interactive)
  (customize-variable 'find-file-visit-truename))

(defun causal-dired--customize-dired-group ()
  "Call the Dired customization group."
  (interactive)
  (customize-group "dired"))

(defun causal-dired-about-dired ()
  "Causal Dired is an opinionated user interface for the Emacs file manager Dired.

Learn more about using Causal Dired at our discussion group on GitHub.
Any questions or comments about Causal should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal Dired, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal Dired was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Causal Dired.

Always choose love."
  (ignore))

(defun causal-dired-about ()
  "About information for Causal Dired."
  (interactive)
  (describe-function #'causal-dired-about-dired))

(provide 'causal-dired-settings)
;;; causal-dired-settings.el ends here
