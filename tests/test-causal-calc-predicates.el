;;; test-causal-calc-predicates.el --- Test Causal Predicates  -*- lexical-binding: t; -*-

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
(require 'causal-calc-predicates)

(ert-deftest test-causal-calc-matrixp ()
  (causalt-calc-setup)
  (calc-push-list '((vec (vec 2 3) (vec 5 7))))
  (should (causal-calc-matrixp))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc-square-matrixp ()
  (causalt-calc-setup)
  (calc-push-list '((vec (vec 2 3) (vec 5 7))))
  (should (causal-calc-square-matrixp))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc-vectorp ()
  (causalt-calc-setup)
  (calc-push-list '((vec 8 2 9)))
  (should (causal-calc-vectorp))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc-crossp ()
  (causalt-calc-setup)
  (calc-push-list '((vec 8 2 9) (vec 1 5 3)))
  (should (causal-calc-crossp))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc-matrixmultp ()
  (causalt-calc-setup)
  (calc-push-list '((vec (vec 8 2)) (vec (vec 7) (vec 4))))
  (should (causal-calc-matrixmultp))
  (causalt-calc-breakdown t))

(provide 'test-causal-calc-predicates)
;;; test-causal-calc-predicates.el ends here
