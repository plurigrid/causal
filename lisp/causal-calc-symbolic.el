;;; causal-calc-symbolic.el --- Causal Symbolic Menu      -*- lexical-binding: t; -*-

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
(require 'causal-calc--calc)
(require 'transient)
(require 'causal-lib)
(require 'causal-calc-labels)
(require 'causal-calc-angle-measure)
(require 'causal-calc-graphics)
(require 'causal-calc-algebra)
(require 'causal-calc-variables)
(require 'causal-calc-fileio)
(require 'causal-calc-utils)

(transient-define-prefix causal-calc-symbolic-tmenu ()
  "Computer Algebra Menu.
\nCommands to work with algebraic expressions. From here you can
- Manipulate algebraic expressions
- Manipulate polynomial expressions
- Perform Calculus
- Solve expressions either symbolically or numerically
- Curve fit data
- Perform summations"
  ["Computer Algebra"
   ["Manipulation"
    :pad-keys t
    ("E" "Simplify" causal-calc--alg-evaluate :transient t)
    ("=" "Evaluate Variables" causal-calc--evaluate :transient t)
    ("m" "⋯›" causal-calc-symbolic-manipulation-tmenu :transient nil)]
   [""
    :pad-keys t
    ("F" "Formula›" causal-calc-subformula-tmenu :transient nil)]
   causal-calc-operators-group]

  [["Polynomial"
    :pad-keys t
    ("f" "Factor" causal-calc--factor :transient t)
    ("e" "Expand" causal-calc--expand :transient t)
    ("p" "⋯›" causal-calc-polynomial-tmenu :transient nil)]

   ["Calculus"
    :pad-keys t
    ("d" "Derivative…" causal-calc--derivative :transient t)
    ("i" "Integral…" causal-calc--integral :transient t)
    ("c" "⋯›" causal-calc--calculus-tmenu :transient nil)]

   ["Solve"
    ("s" "Symbolic›" causal-calc-solve-symbolic-tmenu :transient nil)
    ("n" "Numeric›" causal-calc-solve-numeric-tmenu :transient nil)]]

  [""
   ["Misc"
    ("C" "Curve Fit›" causal-calc-curve-fit-tmenu :transient nil)
    ("S" "Summations›" causal-calc-summations-tmenu :transient nil)
    ("l" "Equalities & Logic›" causal-calc-symbolic-logic-tmenu :transient nil)
    ("g" "Graphics›" causal-calc-plot-tmenu :transient nil)
    ("z" "Variables›" causal-calc-variable-crud-tmenu :transient nil)]

   ["Settings"
    ("A" calc-algebraic-mode
     :description (lambda ()
                    (causal-lib-checkbox-label calc-algebraic-mode
                                            "Algebraic Mode"))
     :transient t)
    ("M" calc-symbolic-mode :description causal-calc-symbolic-mode-label :transient t)
    ("a" causal-calc-angle-measure-tmenu
     :description (lambda ()
                    (format "Angle Measure (now %s)›"
                            (causal-calc-angle-mode-label)))
     :transient t)]]

  causal-calc-navigation-group)

(transient-define-prefix causal-calc-subformula-tmenu ()
  "Sub-formula Menu.
\nCommands to select and edit sub-formulas."
  ["Selection & Navigation"
   [("s" "Select" causal-calc--select-here :transient t)
    ("o" "Select Once" causal-calc--select-once :transient t)
    ("m" "More" causal-calc--select-more :transient t)
    ("p" "Previous" causal-calc--select-previous :transient t)]

   [("u" "Unselect" causal-calc--unselect :transient t)
    ("c" "Clear Selections" causal-calc--clear-selections :transient t)
    ("l" "Less" causal-calc--select-less :transient t)
    ("n" "Next" causal-calc--select-next :transient t)]]

  ["Manipulate"
   [("b" "← Commute" causal-calc--commute-left :transient t)
    ("d" "Distribute" causal-calc--sel-distribute :transient t)
    ("i" "Isolate" causal-calc--sel-isolate :transient t)
    ("N" "Negate" causal-calc--sel-negate :transient t)
    ("e" "⇄" causal-calc--sel-jump-equals :transient t)]

   [("f" "→ Commute" causal-calc--commute-right :transient t)
    ("M" "Merge" causal-calc--sel-merge :transient t)
    ("&" "Invert" causal-calc--sel-invert :transient t)
    ("=" "=" causal-calc--sel-evaluate :transient t)]]

  ["Edit"
   [("`" "Edit" causal-calc--edit-selection :transient nil)
    ("C" "Copy" causal-calc--copy-selection :transient t)]

   [("'" "Replace" causal-calc--enter-selection :transient nil)
    ("D" "Delete" causal-calc--del-selection :transient t)]]

  ;; ["Both Sides"
  ;;  ("*" "Multiply…" causal-calc--sel-mult-both-sides :transient t)
  ;;  ("/" "Divide…" causal-calc--sel-div-both-sides :transient t)
  ;;  ("+" "Add…" causal-calc--sel-add-both-sides :transient t)
  ;;  ("-" "Subtract…" causal-calc--sel-sub-both-sides :transient t)
  ;;  ]

  causal-calc-navigation-group)

(transient-define-prefix causal-calc-symbolic-manipulation-tmenu ()
  "Symbolic Manipulation Menu.
Commands to manipulate a symbolic expression."
  [["Symbolic Manipulation"
   ("E" "Simplify" causal-calc--alg-evaluate :transient t)
   ("=" "Evaluate Variables" causal-calc--evaluate :transient t)
   ("e" "Expand Formula" causal-calc--expand-formula :transient t)
   ("m" "Map Equation" causal-calc--map-equation :transient t)
   ("s" "Substitute" causal-calc--substitute :transient t)]
   causal-calc-operators-group]

  causal-calc-navigation-group)


(transient-define-prefix causal-calc-polynomial-tmenu ()
  "Polynomial Menu.
Commands to manipulate a polynomial expression."
  [["Polynomials"
   ("f" "Factor" causal-calc--factor :transient t)
   ("e" "Expand" causal-calc--expand :transient t)
   ("c" "Collect…" causal-calc--collect :transient t)
   ("a" "Apart" causal-calc--apart :transient t)
   ("n" "Normalize Ratio" causal-calc--normalize-rat :transient t)
   ("\\" "Polynomial Divide" causal-calc--poly-div :transient t)
   ("%" "Polynomial Remainder" causal-calc--poly-rem :transient t)
   ("/" "Polynomial Divide & Remainder" causal-calc--poly-div-rem :transient t)
   ("g" "Polynomial GCD" causal-calc--poly-gcd :transient t)]]
  causal-calc-navigation-group)

(transient-define-prefix causal-calc--calculus-tmenu ()
  "Calculus Menu.
Commands to perform Calculus."
  [["Calculus"
   ("n" "Numeric Integral…" causal-calc--num-integral :transient t)
   ("t" "Taylor…" causal-calc--taylor :transient t)
   ("d" "Derivative…" causal-calc--derivative :transient t)
   ("i" "Integral…" causal-calc--integral :transient t)]
   causal-calc-operators-group]

  causal-calc-navigation-group)

(transient-define-prefix causal-calc-solve-symbolic-tmenu ()
  "Symbolic Solve Menu.
Commands to solve an algebraic expression symbolically."
  [["Symbolic Solutions"
   ("s" "Solve for…" causal-calc--solve-for :transient t)
   ("p" "Polynomial roots for…" causal-calc--poly-roots :transient t)]
   causal-calc-operators-group]

  causal-calc-navigation-group)

(transient-define-prefix causal-calc-solve-numeric-tmenu ()
  "Numerica Solve Menu.
Commands to solve an algebraic expression numerically."
  [["Numerical Solutions"
   ("r" "Find Root" causal-calc--find-root :transient t)
   ("m" "Find Minimum…" causal-calc--find-minimum :transient t)
   ("x" "Find Maximum…" causal-calc--find-maximum :transient t)
   ("h" "Head" causal-calc--head :transient t)
   ("w" "Why" causal-calc--why :transient t)]
   causal-calc-operators-group]

  causal-calc-navigation-group)

(transient-define-prefix causal-calc-curve-fit-tmenu ()
  "Curve Fit Menu.
Curve fit commands."
  [["Curve Fit"
   ("c" "Curve Fit" causal-calc--curve-fit :transient t)
   ("p" "Polynomial Interpolation" causal-calc--poly-interp :transient t)
   ("o" "Open Curve Fit Data…" causal-calc-read-curvefit-data :transient t)]
   causal-calc-operators-group]

  causal-calc-navigation-group)

(transient-define-prefix causal-calc-summations-tmenu ()
  "Summations Menu.
Summation commands."
  [["Summations"
   ("s" "𝚺" causal-calc--summation :transient t)
   ("a" "𝚺 alternating" causal-calc--alt-summation :transient t)
   ("p" "𝚷" causal-calc--product :transient t)
   ("t" "Tabulate" causal-calc--tabulate :transient t)]
   causal-calc-operators-group]

  causal-calc-navigation-group)

(transient-define-prefix causal-calc-symbolic-logic-tmenu ()
  "Symbolic Logic Menu.
Symbolic logic commands."
  ["Equalities"
   [("=" "=" causal-calc--equal-to :transient t)
    ("l" "<" causal-calc--less-than :transient t)
    ("g" ">" causal-calc--greater-than :transient t)]

   [("n" "≠" causal-calc--not-equal-to :transient t)
    ("L" "≤" causal-calc--less-equal :transient t)
    ("G" "≥" causal-calc--greater-equal :transient t)]

   [("x" "Remove Comparator" causal-calc--remove-equal :transient t)]
   causal-calc-operators-group]

  [["Operators"
    ("!" "not (!)" causal-calc--logical-not :transient t)
    ("&" "⋀ (&&)" causal-calc--logical-and :transient t)
    ("|" "⋁ (||)" causal-calc--logical-or :transient t)]

   ["Misc"
    ("e" "∈" causal-calc--in-set :transient t)
    ("i" "if" causal-calc--logical-if :transient t)]]

  causal-calc-navigation-group)


(provide 'causal-calc-symbolic)
;;; causal-calc-symbolic.el ends here
