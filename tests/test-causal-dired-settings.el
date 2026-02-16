;;; test-causal-dired-settings.el --- Causal Dired Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-dired-test-utils)
(require 'causal-dired-settings)

(ert-deftest test-causal-dired-settings-tmenu-bindings ()
  (causalt-dired-setup)
  (let ((test-vectors (list)))
    (push (causalt-suffix-test-vector "T" #'causal-dired--customize-delete-by-moving-to-trash) test-vectors)
    (push (causalt-suffix-test-vector "l" #'causal-dired--customize-dired-use-ls-dired) test-vectors)
    (push (causalt-suffix-test-vector "r" #'causal-dired--customize-dired-auto-revert-buffer) test-vectors)
    (push (causalt-suffix-test-vector "t" #'causal-dired--customize-dired-dwim-target) test-vectors)
    (push (causalt-suffix-test-vector "s" #'causal-dired--customize-dired-listing-switches) test-vectors)
    (push (causalt-suffix-test-vector "c" #'causal-dired--customize-causal-dired-listing-switches) test-vectors)
    (push (causalt-suffix-test-vector "p" #'causal-dired--customize-wdired-allow-to-change-permissions) test-vectors)
    (push (causalt-suffix-test-vector "v" #'causal-dired--customize-find-file-visit-truename) test-vectors)
    (push (causalt-suffix-test-vector "L" #'causal-dired--customize-wdired-allow-to-redirect-links) test-vectors)
    (push (causalt-suffix-test-vector "u" #'causal-lib-customize-causal-lib-use-unicode) test-vectors)
    (push (causalt-suffix-test-vector "R" #'causal-dired--customize-dired-vc-rename-file) test-vectors)
    (push (causalt-suffix-test-vector "d" #'causal-dired--customize-dired-group) test-vectors)
    (push (causalt-suffix-test-vector "a" #'causal-dired-about) test-vectors)

    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-dired-settings-tmenu
                                     '(lambda () (random 5000))))
  (causalt-dired-breakdown t))

(provide 'test-causal-dired-settings)
;;; test-causal-dired-setttings.el ends here
