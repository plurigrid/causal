;;; causal-compile-settings.el --- Causal Compile Settings -*- lexical-binding: t; -*-

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
(require 'compile)
(require 'causal-lib)

(transient-define-prefix causal-compile-settings-tmenu ()
  "Causal Compile settings menu."
  ["Causal Compile Settings"
   [("s" "Scroll Output" causal-compile--customize-compilation-scroll-output
     :description (lambda ()
                    (if compilation-scroll-output
                        (format "Scroll Output (%s)" compilation-scroll-output)
                      "Scroll Output (unset)")))

    ("e" "Autojump First Error"
     causal-compile--customize-compilation-auto-jump-to-first-error
     :description (lambda ()
                    (causal-lib-checkbox-label
                     compilation-auto-jump-to-first-error
                     "Autojump First Error")))
    ("m" "Max Output Line Length"
     causal-compile--customize-compilation-max-output-line-length
     :description (lambda ()
                    (if compilation-max-output-line-length
                        (format "Max Output Line Length (%s)"
                                compilation-max-output-line-length)
                      "Max Output Line Length (unhidden)")))]
   [("G" "Compilation Group" causal-compile--customize-group)
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-compile-about :transient nil)
   (causal-lib-quit-all)])

(defun causal-compile--customize-group ()
  "Customize Compilefile group."
  (interactive)
  (customize-group "compilation"))

(defun causal-compile--customize-compilation-scroll-output ()
  "If non-nil, scroll the *compilation* buffer window as output appears.

Customize `compilation-scroll-output'."
  (interactive)
  (customize-variable 'compilation-scroll-output))

(defun causal-compile--customize-compilation-auto-jump-to-first-error ()
  "If non-nil, automatically jump to the first error during compilation.

Customize `compilation-auto-jump-to-first-error'."
  (interactive)
  (customize-variable 'compilation-auto-jump-to-first-error))

(defun causal-compile--customize-compilation-max-output-line-length ()
  "Output lines that are longer than this value will be hidden.

Customize `compilation-max-output-line-length'."
  (interactive)
  (customize-variable 'compilation-max-output-line-length))

(defun causal-compile-about-compile ()
  "Causal Compile is a Transient menu for `compilation-mode'.

Learn more about using Causal Compile at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal Compile, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal Compile was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Causal Compile.

Always choose love."
  (ignore))

(defun causal-compile-about ()
  "About information for Causal Compile."
  (interactive)
  (describe-function #'causal-compile-about-compile))

(provide 'causal-compile-settings)
;;; causal-compile-settings.el ends here
