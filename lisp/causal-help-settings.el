;;; causal-help-settings.el --- Causal Help Settings -*- lexical-binding: t; -*-

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
(require 'help-mode)
(require 'causal-lib)

(transient-define-prefix causal-help-settings-tmenu ()
  "Causal Help settings menu."
  ["Causal Help: Settings"

   [("G" "Help Group" causal-help--customize-group)
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-help-about :transient nil)
   (causal-lib-quit-all)])

(defun causal-help--customize-group ()
  "Customize Helpfile group."
  (interactive)
  (customize-group "help"))

;; (defun causal-help--customize-man-switches ()
;;   "Switch values passed to the man command, as a single string.

;; Customize `Help-switches'."
;;   (interactive)
;;   (customize-variable 'Help-switches))

;; (defun causal-help--customize-man-prefer-synchronous-call ()
;;   "Whether to call the Un*x \"man\" program synchronously.

;; Customize `Help-prefer-synchronous-call'."
;;   (interactive)
;;   (customize-variable 'Help-prefer-synchronous-call))

;; (defun causal-help--customize-man-support-remote-systems ()
;;   "Whether to call the Un*x \"man\" program on remote systems.

;; Customize `Help-support-remote-systems'."
;;   (interactive)
;;   (customize-variable 'Help-support-remote-systems))


(defun causal-help-about-man ()
  "Causal Help is a Transient menu for `help-mode'.

Learn more about using Causal Help at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal Help, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal Help was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Causal Help.

Always choose love."
  (ignore))

(defun causal-help-about ()
  "About information for Causal Help."
  (interactive)
  (describe-function #'causal-help-about-man))

(provide 'causal-help-settings)
;;; causal-help-settings.el ends here
