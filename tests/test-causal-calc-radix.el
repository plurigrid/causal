;;; test-causal-calc-radix.el --- Causal Radix Tests      -*- lexical-binding: t; -*-

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
(require 'causal-calc-radix)

(ert-deftest test-causal-calc-radix-tmenu ()
  (causalt-calc-setup) ;; calc-number-radix
  (causalt-run-menu-assert-testcases
   'causal-calc-radix-tmenu
   '(("2" () (lambda () (should (= calc-number-radix 2))))
     ("8" () (lambda () (should (= calc-number-radix 8))))
     ("6" () (lambda () (should (= calc-number-radix 16))))
     ;; TODO "n" case
     ("0" () (lambda () (should (= calc-number-radix 10))))))
  (causalt-calc-breakdown t))


(provide 'test-causal-calc-radix)
;;; test-causal-calc-radix.el ends here
