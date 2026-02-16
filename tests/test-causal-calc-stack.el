;;; test-causal-calc-stack.el --- Causal Stack Menu Tests  -*- lexical-binding: t; -*-

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

(ert-deftest test-causal-calc-stack-display-tmenu ()
  (causalt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-left-justify) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-center-justify) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-right-justify) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-truncate-stack) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-truncate-up) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-truncate-down) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-refresh) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-save-modes) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(
                           ("l" . calc-left-justify)
                           ("c" . calc-center-justify)
                           ("r" . calc-right-justify)
                           ("." . calc-truncate-stack)
                           ("p" . calc-truncate-up)
                           ("n" . calc-truncate-down)
                           ("g" . calc-refresh)
                           ("k" . causal-calc-customize-kill-line-numbering)
                           ("s" . calc-save-modes))))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-stack-display-tmenu
                                       '(lambda () (random 5000)))))
  (causalt-calc-breakdown t t))


(provide 'test-causal-calc-stack)
;;; test-causal-calc-stack.el ends here
