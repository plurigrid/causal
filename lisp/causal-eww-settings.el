;;; causal-eww-settings.el --- Causal EWW Settings -*- lexical-binding: t; -*-

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
(require 'eww)
(require 'causal-lib)

(transient-define-prefix causal-eww-settings-tmenu ()
  "Causal EWW settings menu."
  ["Causal EWW: Settings"

   ["Browsing"
    ("r" "Retrieve Command" causal-eww--customize-retrieve-command)
    ("R" "Readable URLs" causal-eww--customize-readable-urls)
    ("h" "History Limit" causal-eww--customize-history-limit
     :description (lambda () (format "History Limit (%d)" eww-history-limit)))]

   ["Directory"
    ("d" "Download" causal-eww--customize-download-directory
     :description (lambda () (format "Directory (%s)" (eww--download-directory))))
    ("b" "Bookmarks" causal-eww--customize-bookmarks-directory
     :description (lambda () (format "Directory (%s)" eww-bookmarks-directory)))]

   ["Display"
    ("f" "Use Fonts" causal-eww--customize-shr-use-fonts
     :description (lambda () (causal-lib-checkbox-label
                         shr-use-fonts "Use Fonts"))
     :transient t)
    ("c" "Use Colors" causal-eww--customize-shr-use-colors
     :description (lambda () (causal-lib-checkbox-label
                         shr-use-colors "Use Colors"))
     :transient t)
    ("i" "Inhibit Images" causal-eww--customize-shr-inhibit-images
     :description (lambda () (causal-lib-checkbox-label
                         shr-inhibit-images "Inhibit Images"))
     :transient t)]]

  ["General"
   :class transient-row
   (causal-lib-customize-unicode)
   (causal-lib-customize-hide-navigation)]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-eww-about :transient nil)
   ("G" "EWW Group" causal-eww--customize-group)
   (causal-lib-quit-all)])

(defun causal-eww--customize-group ()
  "Customize EWW group."
  (interactive)
  (customize-group "eww"))

(defun causal-eww--customize-retrieve-command ()
  "Customize `eww-retrieve-commmand'."
  (interactive)
  (customize-variable 'eww-retrieve-command))

(defun causal-eww--customize-readable-urls ()
  "Customize `eww-readable-urls'."
  (interactive)
  (customize-variable 'eww-readable-urls))

(defun causal-eww--customize-download-directory ()
  "Customize `eww-download-directory'."
  (interactive)
  (customize-variable 'eww-download-directory))

(defun causal-eww--customize-bookmarks-directory ()
  "Customize `eww-bookmarks-directory'."
  (interactive)
  (customize-variable 'eww-bookmarks-directory))

(defun causal-eww--customize-history-limit ()
  "Customize `eww-history-limit'."
  (interactive)
  (customize-variable 'eww-history-limit))

(defun causal-eww--customize-shr-use-fonts ()
  "Customize `shr-use-fonts'."
  (interactive)
  (customize-variable 'shr-use-fonts))

(defun causal-eww--customize-shr-use-colors ()
  "Customize `shr-use-colors'."
  (interactive)
  (customize-variable 'shr-use-colors))

(defun causal-eww--customize-shr-inhibit-images ()
  "Customize `shr-inhibit-images'."
  (interactive)
  (customize-variable 'shr-inhibit-images))

(defun causal-eww-about-eww ()
  "Causal EWW is a Transient menu for Emacs Web Wowser (EWW).

Learn more about using Causal EWW at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal EWW, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal EWW was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Causal EWW.

Always choose love."
  (ignore))

(defun causal-eww-about ()
  "About information for Causal EWW."
  (interactive)
  (describe-function #'causal-eww-about-eww))

(provide 'causal-eww-settings)
;;; causal-eww-settings.el ends here
