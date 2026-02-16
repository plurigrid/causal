;;; test-causal-calc-graphics.el --- Causal Graphics Tests  -*- lexical-binding: t; -*-

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

;; Note that these are UI tests. Testing wrapped functions means effectively
;; testing Calc behavior which is out of scope for testing here.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'causal-calc-test-utils)
(require 'causal-calc-graphics)

(ert-deftest test-causal-calc--push-natural-interval-0-100 ()
  (causalt-calc-setup)
  (causal-calc--push-natural-interval-0-100)
  (should (equal '(intv 3 0 100) (calc-top)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--push-natural-interval-0-360 ()
  (causalt-calc-setup)
  (causal-calc--push-natural-interval-0-360)
  (should (equal '(intv 3 0 360) (calc-top)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--push-float-interval-0-100 ()
  (causalt-calc-setup)
  (causal-calc--push-float-interval-0-100)
  (should (equal '(intv 3 (float 0 0) (float 1 2)) (calc-top)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--push-float-interval-1-symmetric ()
  (causalt-calc-setup)
  (causal-calc--push-float-interval-1-symmetric)
  (should (equal '(intv 3 (float -1 0) (float 1 0)) (calc-top)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--push-sin ()
  (causalt-calc-setup)
  (causal-calc--push-sin)
  (should (equal '(calcFunc-sin (var x var-x)) (calc-top)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--push-cos ()
  (causalt-calc-setup)
  (causal-calc--push-cos)
  (should (equal '(calcFunc-cos (var x var-x)) (calc-top)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--push-tan ()
  (causalt-calc-setup)
  (causal-calc--push-tan)
  (should (equal '(calcFunc-tan (var x var-x)) (calc-top)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--push-ln ()
  (causalt-calc-setup)
  (causal-calc--push-ln)
  (should (equal '(calcFunc-ln (var x var-x)) (calc-top)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--push-e-raised-to-x ()
  (causalt-calc-setup)
  (causal-calc--push-e-raised-to-x)
  (should (equal '(^ (var e var-e) (var x var-x)) (calc-top)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--push-polynomial-order-2 ()
  (causalt-calc-setup)
  (causal-calc--push-polynomial-order-2)
  (should (equal '(+ (^ (var x var-x) 2) 1) (calc-top)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--push-polynomial-order-3 ()
  (causalt-calc-setup)
  (causal-calc--push-polynomial-order-3)
  (should (equal '(+ (+ (^ (var x var-x) 3) (^ (var x var-x) 2)) 1) (calc-top)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc-plot-tmenu ()
  (causalt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-graph-plot) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-graph-clear) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-graph-print) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-graph-command) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'causal-calc-read-plot-data) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("a" . causal-calc--graph-add)
                           ("e" . causal-calc--graph-add-equation)
                           ("A" . causal-calc--graph-add-3d)
                           ("d" . causal-calc--graph-delete)
                           ("N" . causal-calc--graph-name)
                           ("j" . causal-calc--graph-juggle)
                           ("s" . causal-calc-curve-style-tmenu)
                           ("o" . causal-calc-read-plot-data)
                           ("c" . calc-graph-clear)
                           ("r" . calc-graph-plot) ; can't get this to work, dunno why
                           ("n" . causal-calc--graph-num-points)
                           ("S" . causal-calc-plot-options-tmenu)
                           ("g" . causal-calc-graph-settings-tmenu)
                           ("p" . calc-graph-print)
                           ("C" . calc-graph-command)
                           ("E" . causal-calc-graph-examples-tmenu)))
           (test-vectors (append test-vectors causalt-test-operators-group)))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-plot-tmenu
                                       '(lambda () (random 5000)))))
  (causalt-calc-breakdown t t))


(ert-deftest test-causal-calc-graph-examples-tmenu ()
  (causalt-calc-setup)
  (let* ((test-vectors '(("a" . causal-calc--push-natural-interval-0-100)
                         ("b" . causal-calc--push-natural-interval-0-360)
                         ("c" . causal-calc--push-float-interval-0-100)
                         ("d" . causal-calc--push-float-interval-1-symmetric)
                         ("1" . causal-calc--push-sin)
                         ("2" . causal-calc--push-cos)
                         ("3" . causal-calc--push-tan)
                         ("4" . causal-calc--push-ln)
                         ("5" . causal-calc--push-e-raised-to-x)
                         ("6" . causal-calc--push-polynomial-order-2)
                         ("7" . causal-calc--push-polynomial-order-3)))
         (test-vectors (append test-vectors causalt-test-operators-group)))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-graph-examples-tmenu
                                       '(lambda () (random 5000))))

  (causalt-calc-breakdown t t))


(ert-deftest test-causal-calc-graph-settings-tmenu ()
  (causalt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-graph-device) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-graph-output) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-graph-quit) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("d" . calc-graph-device)
                           ("o" . calc-graph-output)
                           ("Q" . calc-graph-quit))))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-graph-settings-tmenu
                                       '(lambda () (random 5000)))))
  (causalt-calc-breakdown t))


(ert-deftest test-causal-calc-curve-style-tmenu ()
  (causalt-calc-setup)
  (let* ((test-vectors '(("l" . causal-calc--graph-toggle-line-style)
                         ("p" . causal-calc--graph-toggle-point-style))))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-curve-style-tmenu
                                       '(lambda () (random 5000))))
  (causalt-calc-breakdown t))



(provide 'test-causal-calc-graphics)
;;; test-causal-calc-graphics.el ends here
