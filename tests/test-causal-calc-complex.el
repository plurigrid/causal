;;; test-causal-calc-complex.el --- Causal Complex Menu Tests  -*- lexical-binding: t; -*-

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
(require 'causal-calc-complex)

(ert-deftest test-causal-calc-complex-number-tmenu ()
  (causalt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-re) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-im) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-conj) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-argument) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("r" . calc-re)
                           ("i" . calc-im)
                           ("c" . calc-conj)
                           ("a" . calc-argument)))
           (test-vectors (append test-vectors causalt-test-operators-group)))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-complex-number-tmenu
                                       '(lambda () (random 5000)))))
  (causalt-calc-breakdown t t))

(provide 'test-causal-calc-complex)
;;; test-causal-calc-complex.el ends here
