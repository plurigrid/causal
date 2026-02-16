;;; test-causal-calc-logarithmic.el --- Causal Logarithmic Menu Tests  -*- lexical-binding: t; -*-

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
(require 'cl-lib)
(require 'ert)
(require 'causal-calc-test-utils)
(require 'causal-calc-logarithmic)

(ert-deftest test-causal-calc-logarithmic-tmenu ()
  (causalt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-ln) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-lnp1) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-log10) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-log) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-exp) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-expm1) (lambda (x) (interactive)(print "WARNING: override"))))
    (let ((test-vectors '(("l" . calc-ln)
                          ("p" . calc-lnp1)
                          ("1" . calc-log10)
                          ("L" . calc-log)
                          ("^" . calc-exp)
                          ("m" . calc-expm1))))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-logarithmic-tmenu
                                       '(lambda () (random 5000)))))
  (causalt-calc-breakdown t t))

(provide 'test-causal-calc-logarithmic)
;;; test-causal-calc-logarithmic.el ends here
