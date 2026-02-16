;;; causal-editkit-settings.el --- Causal Bookmarks Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools, wp

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
(require 'simple)
(require 'autorevert)
(require 'delsel)
(require 'saveplace)
(require 'savehist)
(require 'causal-lib)

(transient-define-prefix causal-editkit-settings-tmenu ()
  "Causal EditKit settings menu."
  ["Causal EditKit: Settings"
   ["Format/Layout"
    ("f" "Auto-fill Mode" auto-fill-mode
     :transient t
     :description (lambda ()
                    (causal-lib-checkbox-label
                     auto-fill-function
                     "Auto-fill Mode")))

    ("t" "Indent Tabs Mode" indent-tabs-mode
     :transient t
     :description (lambda ()
                    (causal-lib-checkbox-label
                     indent-tabs-mode
                     "Indent Tabs Mode")))

    ("." "Double Space" causal-editkit--customize-sentence-end-double-space
     :description (lambda ()
                    (causal-lib-checkbox-label
                     sentence-end-double-space
                     "Double Space Sentences")))
    ("C" "Fill Column" set-fill-column
     :description (lambda ()
                    (format "Fill Column (%d)" fill-column)))]

   ["Save"
    ("p" "Save Place Mode" causal-editkit--customize-save-place-mode
     :description (lambda ()
                    (causal-lib-checkbox-label
                     save-place-mode
                     "Save Place Mode")))

    ("h" "Save History Mode" causal-editkit--customize-savehist-mode
     :description (lambda ()
                    (causal-lib-checkbox-label
                     savehist-mode
                     "Save History Mode")))]

   ["Misc"
    ("S" "Delete Selection Mode" causal-editkit--customize-delete-selection-mode
     :description (lambda ()
                    (causal-lib-checkbox-label
                     delete-selection-mode
                     "Delete Selection Mode")))

    ("N" "Require Final Newline" causal-editkit--customize-require-final-newline
     :description (lambda ()
                    (causal-lib-checkbox-label
                     require-final-newline
                     "Require Final Newline")))


    ("v" "View Read-Only" causal-editkit--customize-view-read-only
     :description (lambda ()
                    (causal-lib-checkbox-label
                     view-read-only
                     "View Read-Only")))]]

  [["Global Auto-Revert Buffers"
    ("F" "File Buffers" causal-editkit--customize-global-auto-revert-mode
     :description (lambda ()
                    (causal-lib-checkbox-label
                     global-auto-revert-mode
                     "File Buffers")))

    ("B" "Non-File Buffers" causal-editkit--customize-global-auto-revert-non-file-buffers
     :description (lambda ()
                    (causal-lib-checkbox-label
                     global-auto-revert-non-file-buffers
                     "Non-File Buffers")))]

   ["Causal"
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-editkit-about :transient nil)
   ("RET" "Done" transient-quit-all)
   (causal-lib-quit-all)])

(defun causal-editkit-about-editkit ()
  "Causal EditKit is a user interface library for Emacs editing commands.

Causal EditKit uses the Transient library to implement its user
interfaces.

Learn more about using Causal EditKit at our discussion
group on GitHub. Any questions or comments about it should be
made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal EditKit, consider making a
modest financial contribution to help support its development and
maintenance. URL `https://www.buymeacoffee.com/kickingvegas'

Causal EditKit was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Causal EditKit.

Always choose love."
  (ignore))

(defun causal-editkit-about ()
  "About information for Causal EditKit."
  (interactive)
  (describe-function #'causal-editkit-about-editkit))

(defun causal-editkit--customize-sentence-end-double-space ()
  "Customize variable `sentence-end-double-space'."
  (interactive)
  (customize-variable 'sentence-end-double-space))

(defun causal-editkit--customize-delete-selection-mode ()
  "Customize variable `delete-selection-mode'."
  (interactive)
  (customize-variable 'delete-selection-mode))

(defun causal-editkit--customize-require-final-newline ()
  "Customize variable `require-final-newline'."
  (interactive)
  (customize-variable 'require-final-newline))

(defun causal-editkit--customize-save-place-mode ()
  "Customize variable `save-place-mode'."
  (interactive)
  (customize-variable 'save-place-mode))

(defun causal-editkit--customize-savehist-mode ()
  "Customize variable `savehist-mode'."
  (interactive)
  (customize-variable 'savehist-mode))

(defun causal-editkit--customize-view-read-only ()
  "Customize variable `view-read-only'."
  (interactive)
  (customize-variable 'view-read-only))

(defun causal-editkit--customize-global-auto-revert-mode ()
  "Customize variable `global-auto-revert-mode'."
  (interactive)
  (customize-variable 'global-auto-revert-mode))

(defun causal-editkit--customize-global-auto-revert-non-file-buffers ()
  "Customize variable `global-auto-revert-non-file-buffers'."
  (interactive)
  (customize-variable 'global-auto-revert-non-file-buffers))

(provide 'causal-editkit-settings)
;;; causal-editkit-settings.el ends here
