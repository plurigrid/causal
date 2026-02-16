;;; causal-re-builder-settings.el --- Causal Re-Builder Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Charles Y. Choi

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
(require 're-builder)
(require 'causal-lib)


(transient-define-prefix causal-re-builder-settings-tmenu ()
  "Causal RE-Builder settings menu."
  ["RE-Builder: Settings"
   ["Customize"
    ("G" "RE-Builder Group" causal-re-builder--customize-group)
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)]]

  [:class transient-row
          (causal-lib-quit-one)
          ("a" "About" causal-re-builder-about :transient nil)

          (causal-lib-quit-all)])

(defun causal-re-builder--customize-group ()
  "Customize Re-Builder group."
  (interactive)
  (customize-group "re-builder"))

(defun causal-re-builder-about-re-builder ()
  "Causal RE-Builder is a Transient menu for RE-Builder.

Learn more about using Causal RE-Builder at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal RE-Builder, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal RE-Builder was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Causal RE-Builder.

Always choose love."
  (ignore))

(defun causal-re-builder-about ()
  "About information for Causal RE-Builder."
  (interactive)
  (describe-function #'causal-re-builder-about-re-builder))

(provide 'causal-re-builder-settings)
;;; causal-re-builder-settings.el ends here
