;;; test-causal-dired-utils.el --- Causal Dired Utils Tests  -*- lexical-binding: t; -*-

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
(require 'causal-dired-utils)

(ert-deftest test-causal-dired-utils-tmenu-bindings ()
  (causalt-dired-setup)

  (let ((test-vectors (list)))
    (push (causalt-suffix-test-vector "F" #'dired-do-find-marked-files) test-vectors)
    (push (causalt-suffix-test-vector "Z" #'dired-do-compress) test-vectors)

    (push (causalt-suffix-test-vector "u" #'dired-upcase) test-vectors)
    (push (causalt-suffix-test-vector "d" #'dired-downcase) test-vectors)

    (push (causalt-suffix-test-vector "s" #'causal-dired-search-replace-tmenu) test-vectors)
    (push (causalt-suffix-test-vector "e" #'causal-dired-elisp-tmenu) test-vectors)
    (push (causalt-suffix-test-vector "l" #'causal-dired-link-tmenu) test-vectors)

    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-dired-utils-tmenu
                                     '(lambda () (random 5000))))
  (causalt-dired-breakdown t))

(ert-deftest test-causal-dired-search-replace-tmenu-bindings ()
  (causalt-dired-setup)
  (cl-letf ((causalt-mock #'dired-do-find-regexp)
            (causalt-mock #'dired-do-isearch)
            (causalt-mock #'dired-do-isearch-regexp)
            (causalt-mock #'dired-do-search)
            (causalt-mock #'dired-do-query-replace-regexp)
            (causalt-mock #'dired-do-find-regexp-and-replace)
            (causalt-mock #'rgrep))

    (let ((test-vectors
           '((:binding "C-s" :command dired-do-isearch)
             (:binding "M-s" :command dired-do-isearch-regexp)
             (:binding "s" :command dired-do-search)
             (:binding "r" :command dired-do-query-replace-regexp)
             (:binding "g" :command dired-do-find-regexp)
             (:binding "G" :command dired-do-find-regexp-and-replace)
             (:binding "f" :command rgrep))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-dired-search-replace-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-dired-breakdown))


(ert-deftest test-causal-dired-elisp-tmenu-bindings ()
  (causalt-dired-setup)

  (let ((test-vectors (list)))
    (push (causalt-suffix-test-vector "B" #'dired-do-byte-compile) test-vectors)
    (push (causalt-suffix-test-vector "L" #'dired-do-load) test-vectors)
    (push (causalt-suffix-test-vector "D" #'byte-recompile-directory) test-vectors)
    (push (causalt-suffix-test-vector "c" #'checkdoc-dired) test-vectors)
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-dired-elisp-tmenu
                                     '(lambda () (random 5000))))
  (causalt-dired-breakdown t))

(ert-deftest test-causal-dired-link-tmenu-bindings ()
  (causalt-dired-setup)

  (let ((test-vectors (list)))
    (push (causalt-suffix-test-vector "s" #'dired-do-symlink) test-vectors)
    (push (causalt-suffix-test-vector "S" #'dired-do-symlink-regexp) test-vectors)
    (push (causalt-suffix-test-vector "r" #'dired-do-relsymlink) test-vectors)
    (push (causalt-suffix-test-vector "R" #'dired-do-relsymlink-regexp) test-vectors)
    (push (causalt-suffix-test-vector "h" #'dired-do-hardlink) test-vectors)
    (push (causalt-suffix-test-vector "H" #'dired-do-hardlink-regexp) test-vectors)


    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-dired-link-tmenu
                                     '(lambda () (random 5000))))
  (causalt-dired-breakdown t))

(ert-deftest test-causal-dired-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal (causal-dired-unicode-get :up-arrow) "Up"))
    (should (string-equal (causal-dired-unicode-get :down-arrow) "Down"))
    (should (string-equal (causal-dired-unicode-get :goto) "Goto"))
    (should (string-equal (causal-dired-unicode-get :directory) "Dir"))
    (should (string-equal (causal-dired-unicode-get :file) "File"))
    (should (string-equal (causal-dired-unicode-get :subdir) "Subdir")))

  (let ((causal-lib-use-unicode t))
    (should (string-equal (causal-dired-unicode-get :up-arrow) "↑"))
    (should (string-equal (causal-dired-unicode-get :down-arrow) "↓"))
    (should (string-equal (causal-dired-unicode-get :goto) "→"))
    (should (string-equal (causal-dired-unicode-get :directory) "📁"))
    (should (string-equal (causal-dired-unicode-get :file) "📄"))
    (should (string-equal (causal-dired-unicode-get :subdir) "🗂️"))))

(provide 'test-causal-dired-utils)
;;; test-causal-dired-utils.el ends here
