;;; test-causal-ibuffer-filter.el --- Test Causal IBuffer Filter  -*- lexical-binding: t; -*-

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
(require 'causal-ibuffer-test-utils)
(require 'causal-ibuffer-filter)

(ert-deftest test-causal-ibuffer-filter-tmenu ()
  (causalt-ibuffer-setup)
  (let ((test-vectors (list)))

    (push (causalt-suffix-test-vector " " #'ibuffer-filter-chosen-by-completion) test-vectors)
    (push (causalt-suffix-test-vector "r" #'ibuffer-switch-to-saved-filters) test-vectors)
    (push (causalt-suffix-test-vector "a" #'ibuffer-add-saved-filters) test-vectors)
    ;;(push (causalt-suffix-test-vector "g" #'ibuffer-filters-to-filter-group) test-vectors)

    ;; (push (causalt-suffix-test-vector "&" #'ibuffer-and-filter) test-vectors)
    ;; (push (causalt-suffix-test-vector "|" #'ibuffer-or-filter) test-vectors)
    ;; (push (causalt-suffix-test-vector "!" #'ibuffer-negate-filter) test-vectors)
    ;; (push (causalt-suffix-test-vector "t" #'ibuffer-exchange-filters) test-vectors)
    ;; (push (causalt-suffix-test-vector "d" #'ibuffer-decompose-filter) test-vectors)

    ;; (push (causalt-suffix-test-vector "-" #'ibuffer-pop-filter) test-vectors)
    ;; (push (causalt-suffix-test-vector "/" #'ibuffer-filter-disable) test-vectors)
    ;; (push (causalt-suffix-test-vector "s" #'ibuffer-save-filters) test-vectors)
    ;; (push (causalt-suffix-test-vector "x" #'ibuffer-delete-saved-filters) test-vectors)

    (push (causalt-suffix-test-vector "p" #'ibuffer-backward-line) test-vectors)
    (push (causalt-suffix-test-vector "n" #'ibuffer-forward-line) test-vectors)
    (push (causalt-suffix-test-vector "[" #'ibuffer-backward-filter-group) test-vectors)
    (push (causalt-suffix-test-vector "]" #'ibuffer-forward-filter-group) test-vectors)
    (push (causalt-suffix-test-vector "j" #'ibuffer-jump-to-buffer) test-vectors)
    ;; (push (causalt-suffix-test-vector "ê" #'ibuffer-jump-to-filter-group) test-vectors)

    ;; (push (causalt-suffix-test-vector "R" #'causal-ibuffer-switch-to-saved-filter-groups) test-vectors)
    ;; (push (causalt-suffix-test-vector "S" #'ibuffer-save-filter-groups) test-vectors)
    ;; (push (causalt-suffix-test-vector "X" #'ibuffer-delete-saved-filter-groups) test-vectors)

    ;; (push (causalt-suffix-test-vector "D" #'ibuffer-decompose-filter-group) test-vectors)
    ;; (push (causalt-suffix-test-vector "P" #'ibuffer-pop-filter-group) test-vectors)
    ;; (push (causalt-suffix-test-vector "\\" #'ibuffer-clear-filter-groups) test-vectors)

    (push (causalt-suffix-test-vector "," #'causal-ibuffer-settings-tmenu) test-vectors)
    (push (causalt-suffix-test-vector "" #'causal-ibuffer-return-dwim) test-vectors)
    ;; (push (causalt-suffix-test-vector "$" #'ibuffer-toggle-filter-group) test-vectors)

    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-ibuffer-filter-tmenu
                                     '(lambda () (random 5000))))
  (causalt-ibuffer-breakdown t))

(provide 'test-causal-ibuffer-filter)
;;; test-causal-ibuffer-filter.el ends here
