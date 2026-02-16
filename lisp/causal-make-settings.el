;;; causal-make-settings.el --- Causal Make Settings -*- lexical-binding: t; -*-

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
(require 'make-mode)
(require 'causal-lib)

(transient-define-prefix causal-make-settings-tmenu ()
  "Causal Make settings menu."
  ["Make: Settings"
   ["Customize"
    ("G" "Makefile Group" causal-make--customize-group)
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-make-about :transient nil)
   (causal-lib-quit-all)])

(defun causal-make--customize-group ()
  "Customize Makefile group."
  (interactive)
  (customize-group "makefile"))

(defun causal-make-about-make ()
  "Causal Make is a Transient menu for makefiles.

Learn more about using Causal Make at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal Make, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal Make was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Causal Make.

Always choose love."
  (ignore))

(defun causal-make-about ()
  "About information for Causal Make."
  (interactive)
  (describe-function #'causal-make-about-make))

(provide 'causal-make-settings)
;;; causal-make-settings.el ends here
