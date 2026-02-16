;;; test-causal-calc--calc.el --- Causal Wrapped Causal Tests  -*- lexical-binding: t; -*-

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
;;(require 'causal)
(require 'causal-calc-test-utils)
(require 'causal-calc--calc)

;;; Tests

(ert-deftest test-causal-calc--inv ()
  (causalt-calc-setup)
  (calc-push 10)
  (causalt-testbench-calc-fn #'causal-calc--inv
                             '()
                             '(float 1 -1))
  (causalt-calc-breakdown t))


(ert-deftest test-causal-calc--sqrt ()
  (causalt-calc-setup)
  (calc-push 25)
  (causalt-testbench-calc-fn #'causal-calc--sqrt
                             '()
                             5)
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--change-sign ()
  (causalt-calc-setup)
  (calc-push 10)
  (causalt-testbench-calc-fn #'causal-calc--change-sign
                             '()
                             -10)
  (causalt-calc-breakdown t))


(ert-deftest test-causal-calc--power ()
  (causalt-calc-setup)
  (calc-push 2)
  (calc-push 3)
  (causalt-testbench-calc-fn #'causal-calc--power
                             '()
                             8)
  (causalt-calc-breakdown t))


(ert-deftest test-causal-calc--abs ()
  (causalt-calc-setup)
  (calc-push -10)
  (causalt-testbench-calc-fn #'causal-calc--abs
                             '()
                             10)
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--factorial ()
  (causalt-calc-setup)
  (calc-push 10)
  (causalt-testbench-calc-fn #'causal-calc--factorial
                             '()
                             3628800)
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--percent ()
  (causalt-calc-setup)
  (calc-push 10)
  (causalt-testbench-calc-fn #'causal-calc--percent
                             '()
                             '(calcFunc-percent 10))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--percent-change ()
  (causalt-calc-setup)
  (calc-push 40)
  (calc-push 50)
  (causalt-testbench-calc-fn #'causal-calc--percent-change
                             '()
                             '(calcFunc-percent 25))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--pi ()
  (causalt-calc-setup)
  (causalt-testbench-calc-fn #'causal-calc--pi
                             '()
                             '(float 314159265359 -11))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--evaluate ()
  (causalt-calc-setup)
  (calc-push '(* 10 (var x var-x)))
  (causalt-testbench-calc-fn #'causal-calc--evaluate
                             '()
                             '(* 10 (var x var-x)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--stack-swap ()
  (causalt-calc-setup)
  (calc-push 10)
  (calc-push 20)
  (causalt-testbench-calc-fn #'causal-calc--stack-swap
                             '()
                             10)
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--stack-drop ()
  (causalt-calc-setup)
  (calc-push 19)
  (calc-push 20)
  (causalt-testbench-calc-fn #'causal-calc--stack-drop
                             '()
                             19)
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--stack-last ()
  (causalt-calc-setup)
  (calc-push 225)
  (call-interactively #'calc-pop)
  (causalt-testbench-calc-fn #'causal-calc--stack-last
                             '()
                             225)
  (causalt-calc-breakdown t))

(provide 'test-causal-calc--calc)
;;; test-causal-calc--calc.el ends here
