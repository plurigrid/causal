;;; causal-man-settings.el --- Causal Man Settings -*- lexical-binding: t; -*-

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
(require 'man)
(require 'causal-lib)

(transient-define-prefix causal-man-settings-tmenu ()
  "Causal Man settings menu."
  ["Causal Man: Settings"
   [("s" "Switches" causal-man--customize-man-switches)
    ("S" "Call Synchronously" causal-man--customize-man-prefer-synchronous-call)
    ("r" "Support Remote" causal-man--customize-man-support-remote-systems)]

   [("G" "Man Group" causal-man--customize-group)
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-man-about :transient nil)
   (causal-lib-quit-all)])

(defun causal-man--customize-group ()
  "Customize Manfile group."
  (interactive)
  (customize-group "man"))

(defun causal-man--customize-man-switches ()
  "Switch values passed to the man command, as a single string.

Customize `Man-switches'."
  (interactive)
  (customize-variable 'Man-switches))

(defun causal-man--customize-man-prefer-synchronous-call ()
  "Whether to call the Un*x \"man\" program synchronously.

Customize `Man-prefer-synchronous-call'."
  (interactive)
  (customize-variable 'Man-prefer-synchronous-call))

(defun causal-man--customize-man-support-remote-systems ()
  "Whether to call the Un*x \"man\" program on remote systems.

Customize `Man-support-remote-systems'."
  (interactive)
  (customize-variable 'Man-support-remote-systems))


(defun causal-man-about-man ()
  "Causal Man is a Transient menu for man pages.

Learn more about using Causal Man at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal Man, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal Man was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Causal Man.

Always choose love."
  (ignore))

(defun causal-man-about ()
  "About information for Causal Man."
  (interactive)
  (describe-function #'causal-man-about-man))

(provide 'causal-man-settings)
;;; causal-man-settings.el ends here
