;;; test-causal-calc.el --- Tests for Causal Calc  -*- lexical-binding: t; -*-

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

;; Test suite for Causal package.

;; Note that tests are focused on verifying UI behavior and not intended to test
;; the `calc' package.

;; * Running the test suite

;; ** Command Line
;; Causal uses `ert' to manage its test suite. The test suite is intended to be
;; run in batch mode via a Makefile in the same directory as this file.
;; Invoke the following command in a shell to run the test suite.

;;     $ make tests

;; ** Interactive
;; Tests can be interactively run via `ert' by loading the following two files:
;; - causal-calc-test-utils.el
;; - test-causal.el

;; Refer to `ert' documentation for more detail on how use it.

;;; Code:
(require 'ert)
(require 'causal-calc)
(require 'causal-calc-test-utils)

;;; Tests

(ert-deftest test-causal-calc-tmenu ()
  (causalt-calc-setup)
  (let ((test-vectors '(("&" . causal-calc--inv)
                        ("Q" . causal-calc--sqrt)
                        ("n" . causal-calc--change-sign)
                        ("^" . causal-calc--power)
                        ("=" . causal-calc--evaluate)
                        ("A" . causal-calc--abs)
                        ("!" . causal-calc--factorial)
                        ("%" . causal-calc--percent-of)
                        ("D" . causal-calc--percent-change)
                        ("p" . causal-calc--pi)
                        ("e" . causal-calc--e-constant)
                        ("+" . causal-calc--plus)
                        ("-" . causal-calc--minus)
                        ("*" . causal-calc--times)
                        ("/" . causal-calc--divide)
                        ("M" . causal-calc--mod)
                        ("m" . causal-calc-modes-tmenu)
                        ("S" . causal-calc-stack-display-tmenu)
                        ("ô" . causal-calc-trail-tmenu)
                        ("o" . causal-calc-rounding-tmenu)
                        ("c" . causal-calc-conversions-tmenu)
                        ("T" . causal-calc-time-tmenu)
                        ("i" . causal-calc-complex-number-tmenu)
                        ("R" . causal-calc-random-number-tmenu)
                        ("t" . causal-calc-trig-tmenu)
                        ("l" . causal-calc-logarithmic-tmenu)
                        ("b" . causal-calc-binary-tmenu)
                        ("v" . causal-calc-vector-tmenu)
                        ("u" . causal-calc-units-tmenu)
                        ("f" . causal-calc-financial-tmenu)
                        ("g" . causal-calc-plot-tmenu)
                        ("s" . causal-calc--stack-swap)
                        ("r" . causal-calc--stack-roll-all)
                        ("d" . causal-calc--stack-drop)
                        ("C" . causal-calc--stack-clear)
                        ("L" . causal-calc--stack-last)
                        ("w" . causal-calc--copy-as-kill)
                        ("z" . causal-calc-variable-crud-tmenu)
                        ("	" . causal-calc-roll-down))))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-tmenu
                                     '(lambda () (random 5000))))
  (execute-kbd-macro "q")
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc-tmenu-last ()
  (causalt-calc-setup)
  (calc-push-list '(2 3))
  (funcall 'causal-calc-tmenu)
  (execute-kbd-macro "^")
  (funcall 'causal-calc-tmenu)
  (execute-kbd-macro "L")
  (should (and (= (calc-top) 3)
               (= (calc-top-n 2) 2)))
  (causalt-calc-breakdown t))

(provide 'test-causal-calc)
;;; test-causal-calc.el ends here
