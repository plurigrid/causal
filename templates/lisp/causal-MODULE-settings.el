;;; causal-$MODULE-settings.el --- Causal $MODULE Settings -*- lexical-binding: t; -*-

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
(require '$MODULE)
(require 'causal-lib)

(transient-define-prefix causal-$MODULE-settings-tmenu ()
  "Causal $MODULE settings menu."
  ["Causal $MODULE: Settings"

   [("G" "$MODULE Group" causal-$MODULE--customize-group)
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-$MODULE-about :transient nil)
   (causal-lib-quit-all)])

(defun causal-$MODULE--customize-group ()
  "Customize $MODULE group."
  (interactive)
  (customize-group "$MODULE"))

(defun causal-$MODULE-about-$MODULE ()
  "Causal $MODULE is a Transient menu for $MODULE pages.

Learn more about using Causal $MODULE at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal $MODULE, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal $MODULE was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Causal $MODULE.

Always choose love."
  (ignore))

(defun causal-$MODULE-about ()
  "About information for Causal $MODULE."
  (interactive)
  (describe-function #'causal-$MODULE-about-$MODULE))

(provide 'causal-$MODULE-settings)
;;; causal-$MODULE-settings.el ends here
