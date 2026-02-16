;;; test-causal-calc-trigonometric.el --- Causal Trig Menu Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords:

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
(require 'causal-calc-trigonometric)

(ert-deftest test-causal-calc-trig-tmenu-integration ()
  (causalt-calc-setup)
  (calc-degrees-mode 1)
  (causalt-run-menu-input-testcases
   'causal-calc-trig-tmenu
   '(("s" (90) 1)
     ("c" (0) 1)
     ("t" (45) (float 1 0))
     ("S" (1) 90)
     ("C" (1) 0)
     ("T" (1) 45)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc-hyperbolic-trig-tmenu-integration ()
  (causalt-calc-setup)
  (calc-degrees-mode 1)
  (causalt-run-menu-input-testcases
   'causal-calc-hyperbolic-trig-tmenu
   '(("s" ((float 88137358702 -11)) (float 1 0))
     ("c" ((float 0 0)) (float 1 0))
     ("t" ((float 33 -2)) (float 318520776903 -12))
     ("S" ((float 1 0)) (float 88137358702 -11))
     ("C" ((float 1 0)) (float 0 0))
     ("T" ((float 318520776903 -12)) (float 33 -2))))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc-trig-tmenu ()
  (causalt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-sin) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-cos) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-tan) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-arcsin) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-arccos) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-arctan) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-to-degrees) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-to-radians) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("s" . calc-sin)
                           ("c" . calc-cos)
                           ("t" . calc-tan)
                           ("S" . calc-arcsin)
                           ("C" . calc-arccos)
                           ("T" . calc-arctan)
                           ("p" . causal-calc--pi)
                           ("d" . calc-to-degrees)
                           ("r" . calc-to-radians)
                           ("a" . causal-calc-angle-measure-tmenu)
                           ("h" . causal-calc-hyperbolic-trig-tmenu)
                           ))
           (test-vectors (append test-vectors causalt-test-operators-group)))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-trig-tmenu
                                       '(lambda () (random 5000)))))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-hyperbolic-trig-tmenu ()
  (causalt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-sinh) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-cosh) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-tanh) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-arcsinh) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-arccosh) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-arctanh) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("s" . calc-sinh)
                           ("c" . calc-cosh)
                           ("t" . calc-tanh)
                           ("S" . calc-arcsinh)
                           ("C" . calc-arccosh)
                           ("T" . calc-arctanh)))
           (test-vectors (append test-vectors causalt-test-operators-group)))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-hyperbolic-trig-tmenu
                                       '(lambda () (random 5000)))))
  (causalt-calc-breakdown t t))

(provide 'test-causal-calc-trigonometric)
;;; test-causal-calc-trigonometric.el ends here
