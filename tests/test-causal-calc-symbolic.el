;;; test-causal-calc-symbolic.el --- Causal Symbolic Tests  -*- lexical-binding: t; -*-

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
(require 'ert)
(require 'causal-calc-test-utils)
(require 'causal-calc-symbolic)

(ert-deftest test-causal-calc-symbolic-tmenu ()
  (causalt-calc-setup)
  (let ((test-vectors '(("E" . causal-calc--alg-evaluate)
                        ("=" . causal-calc--evaluate)
                        ("m" . causal-calc-symbolic-manipulation-tmenu)
                        ("F" . causal-calc-subformula-tmenu)
                        ("f" . causal-calc--factor)
                        ("e" . causal-calc--expand)
                        ("p" . causal-calc-polynomial-tmenu)
                        ("d" . causal-calc--derivative)
                        ("i" . causal-calc--integral)
                        ("c" . causal-calc--calculus-tmenu)
                        ("s" . causal-calc-solve-symbolic-tmenu)
                        ("n" . causal-calc-solve-numeric-tmenu)
                        ("C" . causal-calc-curve-fit-tmenu)
                        ("S" . causal-calc-summations-tmenu)
                        ("l" . causal-calc-symbolic-logic-tmenu)
                        ("g" . causal-calc-plot-tmenu)
                        ;;("A" . calc-algebraic-mode)
                        ;;("M" . calc-symbolic-mode)
                        ("a" . causal-calc-angle-measure-tmenu))))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-symbolic-tmenu
                                     '(lambda () (random 5000))))
  (causalt-calc-breakdown t t))


(ert-deftest test-causal-calc-subformula-tmenu ()
  (causalt-calc-setup)
  (let ((test-vectors '(("s" . causal-calc--select-here)
                        ("o" . causal-calc--select-once)
                        ("m" . causal-calc--select-more)
                        ("p" . causal-calc--select-previous)
                        ("u" . causal-calc--unselect)
                        ("c" . causal-calc--clear-selections)
                        ("l" . causal-calc--select-less)
                        ("n" . causal-calc--select-next)
                        ("b" . causal-calc--commute-left)
                        ("d" . causal-calc--sel-distribute)
                        ("i" . causal-calc--sel-isolate)
                        ("N" . causal-calc--sel-negate)
                        ("e" . causal-calc--sel-jump-equals)
                        ("f" . causal-calc--commute-right)
                        ("M" . causal-calc--sel-merge)
                        ("&" . causal-calc--sel-invert)
                        ("=" . causal-calc--sel-evaluate)
                        ;; ("`" . causal-calc--edit-selection)
                        ("C" . causal-calc--copy-selection)
                        ;; ("'" . causal-calc--enter-selection)
                        ("D" . causal-calc--del-selection))))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-subformula-tmenu
                                     '(lambda () (random 5000))))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-symbolic-manipulation-tmenu ()
  (causalt-calc-setup)
  (let ((test-vectors '(("E" . causal-calc--alg-evaluate)
                        ("=" . causal-calc--evaluate)
                        ("e" . causal-calc--expand-formula)
                        ("m" . causal-calc--map-equation)
                        ("s" . causal-calc--substitute))))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-symbolic-manipulation-tmenu
                                     '(lambda () (random 5000))))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-polynomial-tmenu ()
  (causalt-calc-setup)
  (let ((test-vectors '(("f" . causal-calc--factor)
                        ("e" . causal-calc--expand)
                        ("c" . causal-calc--collect)
                        ("a" . causal-calc--apart)
                        ("n" . causal-calc--normalize-rat)
                        ("\\" . causal-calc--poly-div)
                        ("%" . causal-calc--poly-rem)
                        ("/" . causal-calc--poly-div-rem)
                        ("g" . causal-calc--poly-gcd))))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-polynomial-tmenu
                                     '(lambda () (random 5000))))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc--calculus-tmenu ()
  (causalt-calc-setup)
  (let ((test-vectors '(("n" . causal-calc--num-integral)
                        ("t" . causal-calc--taylor)
                        ("d" . causal-calc--derivative)
                        ("i" . causal-calc--integral))))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc--calculus-tmenu
                                     '(lambda () (random 5000))))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-solve-symbolic-tmenu ()
  (causalt-calc-setup)
  (let ((test-vectors '(("s" . causal-calc--solve-for)
                        ("p" . causal-calc--poly-roots))))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-solve-symbolic-tmenu
                                     '(lambda () (random 5000))))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-solve-numeric-tmenu ()
  (causalt-calc-setup)
  (let ((test-vectors '(("r" . causal-calc--find-root)
                        ("m" . causal-calc--find-minimum)
                        ("x" . causal-calc--find-maximum)
                        ("h" . causal-calc--head)
                        ("w" . causal-calc--why))))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-solve-numeric-tmenu
                                     '(lambda () (random 5000))))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-curve-fit-tmenu ()
  (causalt-calc-setup)
  (let ((test-vectors '(("c" . causal-calc--curve-fit)
                        ("p" . causal-calc--poly-interp))))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-curve-fit-tmenu
                                     '(lambda () (random 5000))))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-summations-tmenu ()
  (causalt-calc-setup)
  (let ((test-vectors '(("s" . causal-calc--summation)
                        ("a" . causal-calc--alt-summation)
                        ("p" . causal-calc--product)
                        ("t" . causal-calc--tabulate))))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-summations-tmenu
                                     '(lambda () (random 5000))))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-symbolic-logic-tmenu ()
  (causalt-calc-setup)
  (let ((test-vectors '(("=" . causal-calc--equal-to)
                        ("l" . causal-calc--less-than)
                        ("g" . causal-calc--greater-than)
                        ("n" . causal-calc--not-equal-to)
                        ("L" . causal-calc--less-equal)
                        ("G" . causal-calc--greater-equal)
                        ("x" . causal-calc--remove-equal)
                        ("!" . causal-calc--logical-not)
                        ("&" . causal-calc--logical-and)
                        ("|" . causal-calc--logical-or)
                        ("e" . causal-calc--in-set)
                        ("i" . causal-calc--logical-if))))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-symbolic-logic-tmenu
                                     '(lambda () (random 5000))))
  (causalt-calc-breakdown t t))

(provide 'test-causal-calc-symbolic)
