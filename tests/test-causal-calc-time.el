;;; test-causal-calc-time.el --- Causal Time Menu Tests   -*- lexical-binding: t; -*-

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
(require 'causal-calc-time)

(ert-deftest test-causal-calc-time-tmenu ()
  (causalt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-now) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-inc-month) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'causal-calc-push-timestamp) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-unix-time) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-business-days-plus) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-business-days-minus) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("n" . calc-now)
                           ("t" . causal-calc-push-timestamp)
                           ("f" . causal-calc-first-day-tmenu)
                           ("i" . calc-inc-month)
                           ("u" . calc-unix-time)
                           ("a" . calc-business-days-plus)
                           ("s" . calc-business-days-minus)))
           (test-vectors (append test-vectors causalt-test-operators-group)))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-time-tmenu
                                       '(lambda () (random 5000)))))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-first-day-tmenu ()
  (causalt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-new-week) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-new-month) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-new-year) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("w" . calc-new-week)
                           ("m" . calc-new-month)
                           ("y" . calc-new-year)))
           (test-vectors (append test-vectors causalt-test-operators-group)))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-first-day-tmenu
                                       '(lambda () (random 5000)))))
  (causalt-calc-breakdown t t))

(provide 'test-causal-calc-time)
;;; test-causal-calc-time.el ends here
