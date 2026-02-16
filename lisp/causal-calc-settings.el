;;; causal-calc-settings.el --- Causal Settings Menu      -*- lexical-binding: t; -*-

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
(require 'calc)
(require 'transient)
(require 'causal-lib)
(require 'causal-calc-labels)

(require 'causal-calc-angle-measure)
(require 'causal-calc-utils)

;; = Menus =
(transient-define-prefix causal-calc-modes-tmenu ()
  "Causal modes menu."
  [["Modes"
    :pad-keys t
    ("A" calc-algebraic-mode
     :description (lambda ()
                    (causal-lib-checkbox-label calc-algebraic-mode
                                               "Algebraic Mode"))
     :transient t)
    ("z" "Leading Zeroes" calc-leading-zeros
     :description (lambda ()
                    (causal-lib-checkbox-label calc-leading-zeros
                                            "Leading Zeroes"))
     :transient t)

    ("F" calc-frac-mode :description causal-calc-prefer-frac-label :transient t)
    ("s" calc-symbolic-mode :description causal-calc-symbolic-mode-label :transient t)
    ("p" calc-polar-mode :description causal-calc-cmplx-or-polar-label :transient t)
    ("c" "Complex Number Format›" causal-calc-complex-format-tmenu
     :description (lambda ()
                    (format "Complex Number Format (now %s)›"
                            (causal-calc-complex-format-label)))
     :transient t)
    ;; ("m" calc-matrix-mode :description causal-calc-matrix-mode-label :transient nil) ; this is really about symbolic computation
    ("P" calc-precision
     :description (lambda ()
                    (format "Precision (now %d)" calc-internal-prec))
     :transient t)
    ("I" "Infinite Mode" causal-calc--infinite-mode
     :description (lambda ()
                    (causal-lib-checkbox-label calc-infinite-mode
                                            "Infinite Mode"))
     :transient t)]

   ["Angular Measure"
    ("a" causal-calc-angle-measure-tmenu
     :description (lambda ()
                    (format "Angle Measure (now %s)›"
                            (causal-calc-angle-mode-label)))
     :transient t)]]

  [["Display"
    ("R" causal-calc-radix-tmenu
     :description (lambda ()
                    (format "Radix (now %s)›" (causal-calc-number-radix-label)))
     :transient t)
    ("f" causal-calc-float-format-tmenu
     :description (lambda ()
                    (format "Float Formats (now %s)›"
                            (causal-calc-float-format-label)))
     :transient t)
    ("g" calc-group-digits
     ;; TODO calc-group-digits can actually be an int 😦
     :description (lambda ()
                    (causal-lib-checkbox-label calc-group-digits
                                            "Show Thousands Separators"))
     :transient t)
    ("," "Thousands Separator…" calc-group-char
     :description (lambda ()
                    (format "Set Thousands Separator (now %s)…" calc-group-char))
     :transient t)
    ("." "Decimal Separator…" calc-point-char
     :description (lambda ()
                    (format "Set Decimal Separator (now %s)…" calc-point-char))
     :transient t)
    ("H" "ℎ𝑚𝑠 Format" calc-hms-notation
     :description (lambda ()
                    (format
                     "Set %s Format (now %s)"
                     (causal-calc-unicode-get :hms)
                     (format calc-hms-format "" "" "")))
     :transient t)]

   ["Settings"
    ("C" "Customize Calc group" causal-calc--customize-group)
    ("S" "Save Calc Settings" calc-save-modes :transient t)
    ("O" "Open Calc Settings File" causal-calc-open-settings-file :transient nil)
    ("C-M-r" "Calc Reset" calc-reset :transient t)
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)]]

  [:class transient-row
          (causal-lib-quit-one)

          ("M-a" "About" causal-calc-about :transient nil)
          (causal-lib-quit-all)])


(defun causal-calc--customize-group ()
  "Customize calc group."
  (interactive)
  (customize-group "calc"))

(transient-define-prefix causal-calc-complex-format-tmenu ()
  "Causal complex formats menu."
  ["Complex Number Format"
   :description (lambda ()
                  (format "Complex Number Format (now %s)"
                          (causal-calc-complex-format-label)))
   ("c" calc-complex-notation
    :description "complex (rectangular) notation"
    :transient nil)

   ("i" calc-i-notation
    :description "𝑖 notation"
    :transient nil)

   ("j" calc-j-notation
    :description "𝑗 notation"
    :transient nil)]

  causal-calc-navigation-group)


(transient-define-prefix causal-calc-float-format-tmenu ()
  "Causal float formats menu."
  ["Float Format (𝑛 is 𝟣: on stack)"
   ("n" "Normal" calc-normal-notation :transient nil)
   ("f" "Fixed Point 𝑛" calc-fix-notation :transient nil)
   ("s" "Scientific" calc-sci-notation :transient nil)
   ("e" "Engineering" calc-eng-notation :transient nil)]

  causal-calc-navigation-group)

;; = Functions =
(defun causal-calc-about-calc ()
  "Causal Calc is an opinionated user interface for the Emacs calculator.

Learn more about using Causal Calc at our discussion group on GitHub.
Any questions or comments about Causal should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal Calc, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal Calc was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Causal Calc.

Always choose love."
  (ignore))

(defun causal-calc-about ()
  "About information for Causal."
  (interactive)
  (describe-function 'causal-calc-about-calc))

(defun causal-calc--infinite-mode ()
  "Toggle infinite mode on or off.

Divide-by-zero (e.g. ‘1 / 0’) results are normally treated as
errors; formulas like this are left in unsimplified form. An
alternate behavior is to treat a divide-by-zero condition as an
infinite result. This command toggles this behavior.

This function is a wrapper over `calc-infinite-mode'.

* References
- info node `(calc) Infinite Mode'
- `calc-infinite-mode'"
  (interactive)
  (call-interactively #'calc-infinite-mode))

(provide 'causal-calc-settings)
;;; causal-calc-settings.el ends here
