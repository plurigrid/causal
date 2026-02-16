;;; causal-calc-trigonometric.el --- Causal Trigonometric Menus  -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'causal-lib)
(require 'causal-calc-utils)
(require 'causal-calc-labels)
(require 'causal-calc-angle-measure)

(transient-define-prefix causal-calc-trig-tmenu ()
  "Causal trigonometric functions menu."
  ;; ["Arguments"
  ;;  ("i" "inverse" "-inverse")
  ;;  ("h" "hyperbolic" "-hyperbolic")]
  ["Trigonometric Functions"
   [("s" "sin" calc-sin
     :description (lambda () (causal-calc-unicode-get :sin))
     :transient t)
    ("c" "cos" calc-cos
     :description (lambda () (causal-calc-unicode-get :cos))
     :transient t)
    ("t" "tan" calc-tan
     :description (lambda () (causal-calc-unicode-get :tan))
     :transient t)]
   [("S" "arcsin" calc-arcsin
     :description (lambda () (causal-calc-unicode-get :arcsin))
     :transient t)
    ("C" "arccos" calc-arccos
     :description (lambda () (causal-calc-unicode-get :arccos))
     :transient t)
    ("T" "arctan" calc-arctan
     :description (lambda () (causal-calc-unicode-get :arctan))
     :transient t)]

   [("d" "To Degrees" calc-to-degrees
     :description (lambda ()
                    (format "%s %s %s"
                            (causal-calc-unicode-get :radians)
                            (causal-calc-unicode-get :to)
                            (causal-calc-unicode-get :degrees)))
     :transient t)
    ("r" "To Radians" calc-to-radians
     :description (lambda ()
                    (format "%s %s %s"
                            (causal-calc-unicode-get :degrees)
                            (causal-calc-unicode-get :to)
                            (causal-calc-unicode-get :radians)))
     :transient t)]]

  [:class transient-row
   ("p" "𝜋" causal-calc--pi
     :description (lambda () (causal-calc-unicode-get :pi))
     :transient t)
   ("a" causal-calc-angle-measure-tmenu
     :description (lambda ()
                    (format "Angle Measure (now %s)›"
                            (causal-calc-angle-mode-label)))
     :transient t)
   ("h" "Hyperbolic›" causal-calc-hyperbolic-trig-tmenu)]

  causal-calc-operators-group-row
  causal-calc-navigation-group)


(transient-define-prefix causal-calc-hyperbolic-trig-tmenu ()
  "Causal hyperbolic trigonometric functions menu."
  [["Hyperbolic"
    ("s" "sinh" calc-sinh :transient t)
    ("c" "cosh" calc-cosh :transient t)
    ("t" "tanh" calc-tanh :transient t)]
   ["Inverse Hyperbolic"
    ("S" "arcsinh" calc-arcsinh :transient t)
    ("C" "arccosh" calc-arccosh :transient t)
    ("T" "arctanh" calc-arctanh :transient t)]
   causal-calc-operators-group]

  causal-calc-navigation-group)

(provide 'causal-calc-trigonometric)
;;; causal-calc-trigonometric.el ends here
