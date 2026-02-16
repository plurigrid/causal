;;; causal-calc-graphics.el --- Causal Graphics Functions  -*- lexical-binding: t; -*-

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
(require 'calc-math) ; needed to reference some symbols not loaded in `calc'.
(require 'transient)
(require 'causal-lib)
(require 'causal-calc-fileio)
(require 'causal-calc-utils)

;; Push Example Functions
(defun causal-calc--push-natural-interval-0-100 ()
  "Push inclusive natural interval [0..100] onto stack."
  (interactive)
  (calc-push-list '((intv 3 0 100))))

(defun causal-calc--push-natural-interval-0-360 ()
  "Push inclusive natural interval [0..360] onto stack.
Convert interval to radians to obtain 2𝜋."
  (interactive)
  (calc-push-list '((intv 3 0 360))))

(defun causal-calc--push-float-interval-0-100 ()
  "Push inclusive float interval 0.0 to 100.0 onto stack."
  (interactive)
  (calc-push-list '((intv 3 (float 0 0) (float 1 2)))))

(defun causal-calc--push-float-interval-1-symmetric ()
  "Push inclusive float interval -1.0 to 1.0 onto stack."
  (interactive)
  (calc-push-list '((intv 3 (float -1 0) (float 1 0)))))

(defun causal-calc--push-sin ()
  "Push sin(x) onto stack."
  (interactive)
  (calc-push-list '((calcFunc-sin (var x var-x)))))

(defun causal-calc--push-cos ()
  "Push cos(x) onto stack."
  (interactive)
  (calc-push-list '((calcFunc-cos (var x var-x)))))

(defun causal-calc--push-tan ()
  "Push tan(x) onto stack."
  (interactive)
  (calc-push-list '((calcFunc-tan (var x var-x)))))

(defun causal-calc--push-ln ()
  "Push ln(x) onto stack."
  (interactive)
  (calc-push-list '((calcFunc-ln (var x var-x)))))

(defun causal-calc--push-e-raised-to-x ()
  "Push e^x onto stack."
  (interactive)
  (calc-push-list '((^ (var e var-e) (var x var-x)))))

(defun causal-calc--push-polynomial-order-2 ()
  "Push x^2 + 1 onto stack.
This can be used as a template formula to edit an order 2 polynomial."
  (interactive)
  (calc-push-list '((+ (^ (var x var-x) 2) 1))))

(defun causal-calc--push-polynomial-order-3 ()
  "Push x^3 + x^2 + 1 onto stack.
This can be used as a template formula to edit an order 3 polynomial."
  (interactive)
  (calc-push-list '((+ (+ (^ (var x var-x) 3) (^ (var x var-x) 2)) 1))))

;; Wrapped Calc Graphics Functions

(defun causal-calc--graph-refresh-plot ()
  "Internal function to refresh the Gnuplot canvas.
This function is intended to be called after a calc-graph command
that changes the internal state of Gnuplot is called.  This way
the canvas is updated to support interactive usage.  Invokes
`calc-graph-plot' to do the actual work."
  (call-interactively #'calc-graph-plot))

(defun causal-calc--graph-add ()
  "Add 2D curve to Gnuplot canvas.
\This function adds the curve specified by the stack
arguments (2:) and (1:), prompting for a name to assign the
curve.

2: x-axis specification
1: y-axis specification

The x-axis specification is typically a vector or an interval.
The y-axis specification can be either a vector or an algebraic
formula with a single independent variable, typically 𝑥.

Invoking this multiple times will for each time generate a new
curve on the same canvas. The last curve generated is referred to
as the current curve. The user can invoke
`causal-calc--graph-juggle' to rotate to an arbitrary curve to
make it current.

The number of sample points used to plot a curve can be set by
calling `causal-calc--graph-num-points'."
  ;; OBSERVATION (kickingvegas): IMHO this is a poor fit for just vector data
  ;; because it separates axis components into separate data structures. Ideally
  ;; one should be able to define a vector [[a b] [c d] [e f]] where [a b],
  ;; [c d], [e f] are three separate points to plot.
  (interactive)
  (call-interactively #'calc-graph-add)
  (call-interactively #'calc-graph-name)
  (causal-calc--graph-refresh-plot))


(defun causal-calc--graph-add-equation ()
  "Add 2D algebraic equation to Gnuplot canvas.
\nThis function adds the curve specified in (1:), prompting the
user for an interval and then a name to assign the curve.

1: y-axis specification, typically an equation

The y-axis specification can be either a vector or an algebraic
formula with a single independent variable, typically 𝑥.

Invoking this multiple times will for each time generate a new
curve on the same canvas. The last curve generated is referred to
as the current curve. The user can invoke
`causal-calc--graph-juggle' to rotate to an arbitrary curve to
make it current.

The number of sample points used to plot a curve can be set by
calling `causal-calc--graph-num-points'."
  ;; OBSERVATION (kickingvegas): IMHO this is a poor fit for just vector data
  ;; because it separates axis components into separate data structures. Ideally
  ;; one should be able to define a vector [[a b] [c d] [e f]] where [a b],
  ;; [c d], [e f] are three separate points to plot.
  (interactive)

  (calc-push (math-read-expr (read-string "Plot Interval: ")))
  (calc-roll-down 2)
  (call-interactively #'calc-graph-add)
  (call-interactively #'calc-graph-name)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-add-3d ()
  "Add 3D curve to Gnuplot canvas.
This function adds the curve specified by the stack arguments 2: and 1:.

3: x-axis specification
2: y-axis specification
1: z-axis specification

The x-axis specification is typically a vector or an interval.
The y-axis specification is typically a vector or an interval.
The z-axis specification can be either a vector or an algebraic formula with two
independent variables, typically 𝑥 and 𝑦.

The number of sample points used to plot a curve can be set by
calling `causal-calc--graph-num-points'."
  ;; OBSERVATION (kickingvegas): IMHO this is a poor fit for just vector data
  ;; because it separates axis components into separate data structures. Ideally
  ;; one should be able to define a vector [[a b c] [d e f] [g h i]] where
  ;; [a b c], [d e f], and [g h i] are three separate points to plot.
  (interactive)
  (call-interactively #'calc-graph-add-3d)
  (call-interactively #'calc-graph-name)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-delete ()
  "Delete the current curve."
  (interactive)
  (call-interactively #'calc-graph-delete)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-hide ()
  "Hide the current curve."
  ;; TODO: for some reason this doesn't work as expected.
  (interactive)
  (call-interactively #'calc-graph-hide)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-num-points ()
  "Define number of sample points to plot for a curve.
Note: on some terminals (aqua, X11) setting this value is only
effective on a curve for the first time; subsequent changes to
this value are not honored."
  ;; TODO: apparently with aquaterm, when sample points are defined for a curve,
  ;; it can not be changed. Need to investigate if this is the same with other
  ;; terminals.
  (interactive)
  (call-interactively #'calc-graph-num-points)
  (causal-calc--graph-refresh-plot))

;; (defun causal-calc--graph-line-style ()
;;   (interactive)
;;   (let* ((linestyle (transient-arg-value "--linestyle=" (transient-args transient-current-command))))
;;          ;;(current-prefix-arg (if linestyle (string-to-number linestyle) nil)))
;;     ;;(call-interactively #'calc-graph-line-style)
;;     (if (not linestyle)
;;         (call-interactively #'calc-graph-line-style)
;;       (calc-graph-line-style (string-to-number linestyle)))
;;     (causal-calc--graph-refresh-plot)))

(defun causal-calc--graph-toggle-line-style ()
  "Toggle whether a line is rendered for the current curve.
Defining a line style is not yet supported in Causal."
  (interactive)
  (call-interactively #'calc-graph-line-style)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-toggle-point-style ()
  "Toggle whether points are rendered for the current curve.
Defining a point style is not yet supported in Causal."
  (interactive)
  (call-interactively #'calc-graph-point-style)
  (causal-calc--graph-refresh-plot))


(defun causal-calc--graph-juggle ()
  "Change the current curve by rotating through the set of added curves.
Note that direct selection of a curve is not yet supported in Causal."
  ;; OBSERVATION (kickingvegas): Calc's notion of juggling is an unfortunate
  ;; design decision because it imposes an unnatural means to access a curve.
  ;; More ideal would a curve abstraction that supports direct manipulation.
  (interactive)
  (call-interactively #'calc-graph-juggle)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-grid ()
  "Toggle canvas grid."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-grid)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-key ()
  "Toggle canvas legend of defined curves."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-key)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-border ()
  "Toggle canvas border."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-border)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-header ()
  "Set string for canvas title."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-header)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-name ()
  "Set string for current curve name.
This string name is used in the canvas legend (key)."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-name)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-title-x ()
  "Set string for x-axis title."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-title-x)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-title-y ()
  "Set string for y-axis title."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-title-y)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-title-z ()
  "Set string for z-axis title."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-title-z)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-zero-x ()
  "Toggle solid line for y=0."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-zero-x)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-zero-y ()
  "Toggle solid line for x=0."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-zero-y)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-log-x ()
  "Toggle linear/log scaling for x-axis."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-log-x)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-log-y ()
  "Toggle linear/log scaling for y-axis."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-log-y)
  (causal-calc--graph-refresh-plot))

(defun causal-calc--graph-log-z ()
  "Toggle linear/log scaling for z-axis."
  ;; TODO: inspect state variable and show current value.
  (interactive)
  (call-interactively #'calc-graph-log-z)
  (causal-calc--graph-refresh-plot))

;; Menus

(transient-define-prefix causal-calc-plot-tmenu ()
  "Causal plot menu."
  ["Graphics"
   ["Curve"
    :pad-keys t
    ("a" "Add 2D Curve" causal-calc--graph-add :transient t)
    ("e" "Add 2D Equation" causal-calc--graph-add-equation :transient t)
    ("A" "Add 3D" causal-calc--graph-add-3d :transient t)
    ("d" "Delete" causal-calc--graph-delete :transient t)
    ("N" "Name…" causal-calc--graph-name :transient t)
    ("j" "Juggle" causal-calc--graph-juggle :transient t)
    ("s" "Style›" causal-calc-curve-style-tmenu :transient nil)
    ("o" "Open Plot Data…" causal-calc-read-plot-data :transient t)]
   ["Canvas"
    ;;("f" "Plot 2D Fast" calc-graph-fast :transient t) ;; obsolete
    ;;("F" "Plot 3D Fast" calc-graph-fast-3d :transient t) ;; obsolete
    ("r" "Redraw" calc-graph-plot :transient t)
    ("c" "Clear" calc-graph-clear :transient t)
    ;;("h" "Toggle Hide" causal-calc--graph-hide :transient t) ; does this even work?
    ("n" "# Data Points…" causal-calc--graph-num-points :transient t)
    ;; force redraw by setting current-prefix-arg to -1 and call calc-graph-plot
    ("S" "Style›" causal-calc-plot-options-tmenu)]
   ["Utils"
    ("g" "Settings›" causal-calc-graph-settings-tmenu :transient nil)
    ("p" "Print" calc-graph-print
     ;:description (lambda () (format "Print (%s)…" calc-gnuplot-print-device))
     :transient nil)
    ("C" "Raw Command…" calc-graph-command :transient nil)]
   causal-calc-operators-group]

  ["Data"
   :pad-keys t
   ("E" "Examples›" causal-calc-graph-examples-tmenu :transient nil)]
  ;;("v" "Vector/Matrix›" causal-calc-vector-tmenu :transient nil)

  causal-calc-navigation-group)


(transient-define-prefix causal-calc-plot-options-tmenu ()
  "Causal plot options menu."
  ["Plot Styles"
   ["Canvas"
    ("t" "Set Title…" causal-calc--graph-header :transient t)
    ("k" "Key" causal-calc--graph-key :transient t)
    ("g" "Grid" causal-calc--graph-grid :transient t)
    ("b" "Border" causal-calc--graph-border :transient t)]

   ["Curve"
    ("n" "Name…" causal-calc--graph-name :transient t)
    ("l" "Toggle Line" causal-calc--graph-toggle-line-style :transient t)
    ("p" "Toggle Point" causal-calc--graph-toggle-point-style :transient t)
    ("j" "Juggle" causal-calc--graph-juggle :transient t)]]

  ["Axes"
   ["Title"
    ("x" "Set 𝑥…" causal-calc--graph-title-x :transient t); refactor to new axis menu
    ("y" "Set 𝑦…" causal-calc--graph-title-y :transient t)
    ("z" "Set 𝑧…" causal-calc--graph-title-z :transient t)]

   ["Log/Linear"
    ("X" "Toggle 𝑥" causal-calc--graph-log-x :transient t)
    ("Y" "Toggle 𝑦" causal-calc--graph-log-y :transient t)
    ("Z" "Toggle 𝑧" causal-calc--graph-log-z :transient t)]

   ["Zero Axis"
    ("1" "Toggle 𝑥" causal-calc--graph-zero-x :transient t); refactor to new menu
    ("2" "Toggle 𝑦" causal-calc--graph-zero-y :transient t)]]

  causal-calc-navigation-group)

(transient-define-prefix causal-calc-graph-examples-tmenu ()
  "Menu of plot examples."
  ["Intervals"
   ["Natural"
    :pad-keys t
    ("a" "insert [0..100]" causal-calc--push-natural-interval-0-100 :transient t)
    ("b" "insert [0..360]" causal-calc--push-natural-interval-0-360 :transient t)]
   ["Float"
    :pad-keys t
    ("c" "insert [0.0 .. 100.0]"
     causal-calc--push-float-interval-0-100 :transient t)
    ("d" "insert [-1.0 .. 1.0]"
     causal-calc--push-float-interval-1-symmetric :transient t)]

   causal-calc-operators-group]

  ["Curves"
   ["Trigonometric"
    :pad-keys t
    ("1" "𝑠𝑖𝑛(𝑥)" causal-calc--push-sin :transient t)
    ("2" "𝑐𝑜𝑠(𝑥)" causal-calc--push-cos :transient t)
    ("3" "𝑡𝑎𝑛(𝑥)" causal-calc--push-tan :transient t)]

   ["Logarithmic"
    :pad-keys t
    ("4" "𝑙𝑛(𝑥)" causal-calc--push-ln :transient t)
    ("5" "𝑒ˣ" causal-calc--push-e-raised-to-x :transient t)]

   ["Polynomial"
    :pad-keys t
    ("6" "𝑥² + 𝟣" causal-calc--push-polynomial-order-2 :transient t)
    ("7" "𝑥³ + 𝑥² + 𝟣" causal-calc--push-polynomial-order-3 :transient t)]]

  causal-calc-navigation-group)

(transient-define-prefix causal-calc-graph-settings-tmenu ()
  "Graphics settings menu."
  ["Graphics Settings"
    ("d" "Set Device…" calc-graph-device
     ;:description (lambda () (format "Device (%s)…" calc-gnuplot-default-device)) this variable only initializes.
     :transient nil)
    ("o" "Set Output File…" calc-graph-output
     ;:description (lambda () (format "Output (%s)…" calc-gnuplot-default-output))
     :transient nil)
    ("Q" "Quit Gnuplot Session" calc-graph-quit :transient nil)]

  causal-calc-navigation-group)

(transient-define-prefix causal-calc-curve-style-tmenu ()
  "Style curve menu."
  ["Style Curve"
   ["Line"
    ("l" "Toggle Line" causal-calc--graph-toggle-line-style :transient t)]
   ["Point"
    ("p" "Toggle Point" causal-calc--graph-toggle-point-style :transient t)]]

  causal-calc-navigation-group)


;; (transient-define-prefix causal-calc-curve-style-tmenu ()
;;   "hey there"
;;   ;;:value '("--linestyle=1")
;;   [
;;    ["Line"
;;     ("L" "linestyle [1..6]" "--linestyle="
;;      :choices ("1" "2" "3" "4" "5"))
;;     ("l" "Toggle Line Style" causal-calc--graph-line-style :transient t) ;; set prefix
;;     ]
;;    ["Point"
;;     ("p" "Toggle Point Style" causal-calc--graph-point-style :transient t) ;; set prefix ]
;;     ]
;;    ]

;;   [:class transient-row
;;           (causal-lib-quit-one)
;;           (causal-lib-quit-all)
;;           (causal-calc-undo-suffix)])

(provide 'causal-calc-graphics)
;;; causal-calc-graphics.el ends here
