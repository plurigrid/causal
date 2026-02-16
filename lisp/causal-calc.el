;;; causal-calc.el --- Transient UI for Calc -*- lexical-binding: t; -*-

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

;; Causal Calc is an opinionated Transient-based user interface for Emacs Calc.

;; INSTALLATION
;; (require 'causal-calc) ; optional if using autoloaded menu
;; (keymap-set calc-mode-map "C-o" #'causal-calc-tmenu)
;; (keymap-set calc-alg-map "C-o" #'causal-calc-tmenu)

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:

(require 'calc)
(require 'calc-math) ; needed to reference some symbols not loaded in `calc'.
(require 'causal-calc--calc)
(require 'transient)
(require 'causal-lib)
(require 'causal-calc-utils)

(require 'causal-calc-binary)
(require 'causal-calc-complex)
(require 'causal-calc-conversion)
(require 'causal-calc-logarithmic)
(require 'causal-calc-random)
(require 'causal-calc-rounding)
(require 'causal-calc-settings)
(require 'causal-calc-time)
(require 'causal-calc-trigonometric)
(require 'causal-calc-units)
(require 'causal-calc-vector)
(require 'causal-calc-graphics)
(require 'causal-calc-trail)
(require 'causal-calc-stack)
(require 'causal-calc-financial)
(require 'causal-calc-symbolic)
(require 'causal-calc-variables)

;; Menus
;;;###autoload (autoload 'causal-calc-tmenu "causal-calc" nil t)
(transient-define-prefix causal-calc-tmenu ()
  "Causal Calc main menu."
  [["Calc"
    ("&" "1/x" causal-calc--inv
     :description (lambda () (causal-calc-unicode-get :inv))
     :transient t)
    ("Q" "√" causal-calc--sqrt
     :description (lambda () (causal-calc-unicode-get :sqrt))
     :transient t)
    ("n" "∓" causal-calc--change-sign
     :description (lambda () (causal-calc-unicode-get :change-sign))
     :transient t)
    ("^" "𝑦ˣ" causal-calc--power
     :description (lambda () (causal-calc-unicode-get :power))
     :transient t)
    ("=" "=" causal-calc--evaluate :transient t)]
   [""
    ("A" "|𝑥|" causal-calc--abs
     :description (lambda () (causal-calc-unicode-get :abs))
     :transient t)
    ("!" " !" causal-calc--factorial
     :description (lambda () (causal-calc-unicode-get :factorial))
     :transient t)
    ("%" "𝑎𝑏%" causal-calc--percent-of :transient t)
    ("D" " Δ%" causal-calc--percent-change
     :description (lambda () (causal-calc-unicode-get :percent-change))
     :transient t)]
   ["Constants"
    ("p" "𝜋" causal-calc--pi
     :description (lambda () (causal-calc-unicode-get :pi))
     :transient t)
    ("e" "𝑒" causal-calc--e-constant
     :description (lambda () (causal-calc-unicode-get :e))
     :transient t)]

   causal-calc-basic-operators-group

   ["Stack"
    ("s" "Swap" causal-calc--stack-swap :transient t)
    ("r" "Roll" causal-calc--stack-roll-all :transient t)
    ("d" "Drop" causal-calc--stack-drop :transient t)
    ("C" "Clear" causal-calc--stack-clear :transient t)]

   [""
    ("L" "Last" causal-calc--stack-last :transient t)
    ("w" "Copy" causal-calc--copy-as-kill :transient nil)
    ("`" "Edit" calc-edit)
    ("z" "Variables›" causal-calc-variable-crud-tmenu)]]

  ["Arithmetic"
   :class transient-row
    ("o" "Rounding›" causal-calc-rounding-tmenu)
    ("c" "Conversion›" causal-calc-conversions-tmenu)
    ("T" "Time›" causal-calc-time-tmenu)
    ("i" "Complex›" causal-calc-complex-number-tmenu)
    ("R" "Random›" causal-calc-random-number-tmenu)]

  ["Functions"
   [("t" "Trigonometric›" causal-calc-trig-tmenu)
    ("l" "Logarithmic›" causal-calc-logarithmic-tmenu)]

   [("b" "Binary›" causal-calc-binary-tmenu)
    ("v" "Vector/Matrix›" causal-calc-vector-tmenu)]

   [("u" "Units›" causal-calc-units-tmenu)
    ("f" "Financial›" causal-calc-financial-tmenu)]

   [("g" "Graphics›" causal-calc-plot-tmenu)
    ("a" "Algebra›" causal-calc-symbolic-tmenu)]]

  ["Settings"
   :class transient-row
   ("m" "Modes, Displays, Angles›" causal-calc-modes-tmenu)
   ("S" "Stack›" causal-calc-stack-display-tmenu)
   ("M-t" "Trail›" causal-calc-trail-tmenu)]

  [:class transient-row
          (causal-lib-quit-one)
          (causal-calc-algebraic-entry)
          (causal-calc-enter)
          (causal-calc-roll-down)
          (causal-calc-pop)
          (causal-calc-undo-suffix)
          ("q" "Quit" calc-quit)])

(provide 'causal-calc)
;;; causal-calc.el ends here
