;;; test-causal-calc-conversion.el --- Causal Conversion Menu Tests  -*- lexical-binding: t; -*-

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
(require 'causal-calc-conversion)

(ert-deftest test-causal-calc-conversions-tmenu ()
  (causalt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-to-degrees) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-to-radians) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-to-hms) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-from-hms) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-float) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-fraction) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("d" . calc-to-degrees)
                           ("r" . calc-to-radians)
                           ("h" . calc-to-hms)
                           ("H" . calc-from-hms)
                           ("f" . calc-float)
                           ("F" . calc-fraction)))
           (test-vectors (append test-vectors causalt-test-operators-group)))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-conversions-tmenu
                                       '(lambda () (random 5000)))))
  (causalt-calc-breakdown t t))

(provide 'test-causal-calc-conversion)
;;; test-causal-calc-conversion.el ends here
