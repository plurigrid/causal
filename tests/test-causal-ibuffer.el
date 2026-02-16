;;; test-causal-ibuffer.el --- Causal IBuffer Tests -*- lexical-binding: t; -*-

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
(require 'causal-lib-test-utils)
(require 'causal-ibuffer)

(ert-deftest test-causal-ibuffer-tmenu-bindings ()
  (causalt-ibuffer-setup)
  (let ((test-vectors (list)))
    (push (causalt-suffix-test-vector "o" #'ibuffer-visit-buffer-other-window) test-vectors)
    (push (causalt-suffix-test-vector "S" #'ibuffer-do-save) test-vectors)
    (push (causalt-suffix-test-vector "D" #'ibuffer-do-delete) test-vectors)
    (push (causalt-suffix-test-vector "=" #'ibuffer-diff-with-file) test-vectors)
    (push (causalt-suffix-test-vector "w" #'ibuffer-copy-filename-as-kill) test-vectors)
    (push (causalt-suffix-test-vector "M" #'causal-ibuffer-operations-tmenu) test-vectors)

    (push (causalt-suffix-test-vector "m" #'ibuffer-mark-forward) test-vectors)
    (push (causalt-suffix-test-vector "t" #'causal-ibuffer-mark-tmenu) test-vectors)
    (push (causalt-suffix-test-vector "r" #'causal-ibuffer-mark-regexp-tmenu) test-vectors)
    (push (causalt-suffix-test-vector "u" #'ibuffer-unmark-forward) test-vectors)
    (push (causalt-suffix-test-vector "d" #'ibuffer-mark-for-delete) test-vectors)
    (push (causalt-suffix-test-vector "x" #'ibuffer-do-kill-on-deletion-marks) test-vectors)
    (push (causalt-suffix-test-vector "U" #'ibuffer-unmark-all-marks) test-vectors)

    (push (causalt-suffix-test-vector "s" #'causal-ibuffer-sortby-tmenu) test-vectors)
    (push (causalt-suffix-test-vector "`" #'ibuffer-switch-format) test-vectors)
    (push (causalt-suffix-test-vector "b" #'ibuffer-bury-buffer) test-vectors)
    (push (causalt-suffix-test-vector "g" #'ibuffer-update) test-vectors)
    ;; (push (causalt-suffix-test-vector "$" #'ibuffer-toggle-filter-group) test-vectors)

    (push (causalt-suffix-test-vector "p" #'ibuffer-backward-line) test-vectors)
    (push (causalt-suffix-test-vector "n" #'ibuffer-forward-line) test-vectors)
    (push (causalt-suffix-test-vector "{" #'ibuffer-backwards-next-marked) test-vectors)
    (push (causalt-suffix-test-vector "}" #'ibuffer-forward-next-marked) test-vectors)
    (push (causalt-suffix-test-vector "[" #'ibuffer-backward-filter-group) test-vectors)
    (push (causalt-suffix-test-vector "]" #'ibuffer-forward-filter-group) test-vectors)
    (push (causalt-suffix-test-vector "j" #'ibuffer-jump-to-buffer) test-vectors)
    ;; (push (causalt-suffix-test-vector "ê" #'ibuffer-jump-to-filter-group) test-vectors)

    (push (causalt-suffix-test-vector " " #'ibuffer-filter-chosen-by-completion) test-vectors)
    (push (causalt-suffix-test-vector "/" #'ibuffer-filter-disable) test-vectors)
    (push (causalt-suffix-test-vector "F" #'causal-ibuffer-filter-tmenu) test-vectors)
    (push (causalt-suffix-test-vector "O" #'ibuffer-do-occur) test-vectors)
    (push (causalt-suffix-test-vector "ò" #'ibuffer-do-query-replace) test-vectors)
    (push (causalt-suffix-test-vector "" #'ibuffer-do-query-replace-regexp) test-vectors)

    (push (causalt-suffix-test-vector "J" #'bookmark-jump) test-vectors)

    (push (causalt-suffix-test-vector "" #'causal-ibuffer-return-dwim) test-vectors)
    (push (causalt-suffix-test-vector "," #'causal-ibuffer-settings-tmenu) test-vectors)
    (push (causalt-suffix-test-vector "q" #'quit-window) test-vectors)

    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-ibuffer-tmenu
                                     '(lambda () (random 5000))))
  (causalt-ibuffer-breakdown t))

(ert-deftest test-causal-ibuffer-operations-tmenu ()
  (causalt-ibuffer-setup)
  (let ((test-vectors (list)))
    (push (causalt-suffix-test-vector "R" #'ibuffer-do-rename-uniquely) test-vectors)
    (push (causalt-suffix-test-vector "!" #'ibuffer-do-shell-command-file) test-vectors)
    (push (causalt-suffix-test-vector "|" #'ibuffer-do-shell-command-pipe) test-vectors)

    ;; (push (causalt-suffix-test-vector "E" #'ibuffer-do-eval) test-vectors)
    (push (causalt-suffix-test-vector "B" #'ibuffer-copy-buffername-as-kill) test-vectors)

    (push (causalt-suffix-test-vector "T" #'ibuffer-do-toggle-read-only) test-vectors)
    (push (causalt-suffix-test-vector "L" #'ibuffer-do-toggle-lock) test-vectors)

    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-ibuffer-operations-tmenu
                                     '(lambda () (random 5000))))
  (causalt-ibuffer-breakdown t))

(ert-deftest test-causal-ibuffer-sortby-tmenu ()
  (causalt-ibuffer-setup)
  (let ((test-vectors (list)))
    (push (causalt-suffix-test-vector "v" #'ibuffer-do-sort-by-recency) test-vectors)
    (push (causalt-suffix-test-vector "a" #'ibuffer-do-sort-by-alphabetic) test-vectors)
    (push (causalt-suffix-test-vector "f" #'ibuffer-do-sort-by-filename/process) test-vectors)

    (push (causalt-suffix-test-vector "m" #'ibuffer-do-sort-by-major-mode) test-vectors)
    (push (causalt-suffix-test-vector "s" #'ibuffer-do-sort-by-size) test-vectors)

    (push (causalt-suffix-test-vector "i" #'ibuffer-invert-sorting) test-vectors)
    (push (causalt-suffix-test-vector "," #'ibuffer-toggle-sorting-mode) test-vectors)
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-ibuffer-sortby-tmenu
                                     '(lambda () (random 5000))))
  (causalt-ibuffer-breakdown t))

(ert-deftest test-causal-ibuffer-mark-tmenu ()
  (causalt-ibuffer-setup)
  (let ((test-vectors (list)))
    (push (causalt-suffix-test-vector "m" #'ibuffer-mark-by-mode) test-vectors)
    (push (causalt-suffix-test-vector "d" #'ibuffer-mark-dired-buffers) test-vectors)
    (push (causalt-suffix-test-vector "h" #'ibuffer-mark-help-buffers) test-vectors)

    (push (causalt-suffix-test-vector "*" #'ibuffer-mark-modified-buffers) test-vectors)
    (push (causalt-suffix-test-vector "r" #'ibuffer-mark-read-only-buffers) test-vectors)
    (push (causalt-suffix-test-vector "u" #'ibuffer-mark-unsaved-buffers) test-vectors)

    (push (causalt-suffix-test-vector "D" #'ibuffer-mark-dissociated-buffers) test-vectors)
    (push (causalt-suffix-test-vector "s" #'ibuffer-mark-special-buffers) test-vectors)
    (push (causalt-suffix-test-vector "z" #'ibuffer-mark-compressed-file-buffers) test-vectors)
    (push (causalt-suffix-test-vector "U" #'ibuffer-unmark-all-marks) test-vectors)

    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-ibuffer-mark-tmenu
                                     '(lambda () (random 5000))))
  (causalt-ibuffer-breakdown t))

(ert-deftest test-causal-ibuffer-mark-regexp-tmenu ()
  (causalt-ibuffer-setup)
  (let ((test-vectors (list)))
    (push (causalt-suffix-test-vector "f" #'ibuffer-mark-by-file-name-regexp) test-vectors)
    (push (causalt-suffix-test-vector "n" #'ibuffer-mark-by-name-regexp) test-vectors)
    (push (causalt-suffix-test-vector "m" #'ibuffer-mark-by-mode-regexp) test-vectors)
    (push (causalt-suffix-test-vector "c" #'ibuffer-mark-by-content-regexp) test-vectors)
    (push (causalt-suffix-test-vector "U" #'ibuffer-unmark-all-marks) test-vectors)

    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-ibuffer-mark-regexp-tmenu
                                     '(lambda () (random 5000))))
  (causalt-ibuffer-breakdown t))

(provide 'test-causal-ibuffer)
;;; test-causal-ibuffer.el ends here
