;;; test-causal-re-builder.el --- Causal Re-Builder Tests -*- lexical-binding: t; -*-

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
(require 'causal-re-builder-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-re-builder)

(ert-deftest test-causal-re-builder-tmenu-bindings ()
  (causalt-setup)
  (let ((test-vectors (list)))
    (push (causalt-suffix-test-vector "w" #'causal-re-builder-copy) test-vectors)
    (push (causalt-suffix-test-vector "g" #'causal-re-builder-grep-copy) test-vectors)
    (push (causalt-suffix-test-vector "c" #'reb-copy) test-vectors)
    (push (causalt-suffix-test-vector "p" #'reb-prev-match) test-vectors)
    (push (causalt-suffix-test-vector "n" #'reb-next-match) test-vectors)
    (push (causalt-suffix-test-vector "x" #'reb-change-syntax) test-vectors)
    (push (causalt-suffix-test-vector "b" #'reb-change-target-buffer) test-vectors)
    (push (causalt-suffix-test-vector "t" #'reb-toggle-case) test-vectors)
    (push (causalt-suffix-test-vector "s" #'reb-enter-subexp-mode) test-vectors)
    (push (causalt-suffix-test-vector "f" #'reb-force-update) test-vectors)
    (push (causalt-suffix-test-vector "," #'causal-re-builder-settings-tmenu) test-vectors)
    (push (causalt-suffix-test-vector "i" #'causal-re-builder-regexp-info) test-vectors)
    (push (causalt-suffix-test-vector "q" #'reb-quit) test-vectors)
    (push (causalt-suffix-test-vector "o" #'causal-reb-occur) test-vectors)
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-re-builder-tmenu
                                     '(lambda () (random 5000))))
  (execute-kbd-macro "")
  (causalt-breakdown t))

(provide 'test-causal-re-builder)
;;; test-causal-re-builder.el ends here
