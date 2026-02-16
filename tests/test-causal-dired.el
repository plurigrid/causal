;;; test-causal-dired.el --- Causal Dired Tests      -*- lexical-binding: t; -*-

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
(require 'causal-dired)

(ert-deftest test-causal-dired-tmenu-bindings ()
  (causalt-dired-setup)

  (cl-letf
      (((symbol-function #'dired-do-shell-command) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'dired-do-async-shell-command) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'image-dired) (lambda (x) (interactive)(print "WARNING: override"))))

    (let ((test-vectors (list))
          (dired-use-ls-dired t))
      (push (causalt-suffix-test-vector "o" #'dired-find-file-other-window) test-vectors)
      (push (causalt-suffix-test-vector "v" #'dired-view-file) test-vectors)
      (push (causalt-suffix-test-vector "C" #'dired-do-copy) test-vectors)
      (push (causalt-suffix-test-vector "R" #'dired-do-rename) test-vectors)
      (push (causalt-suffix-test-vector "D" #'dired-do-delete) test-vectors)
      (push (causalt-suffix-test-vector "l" #'causal-dired-link-tmenu) test-vectors)
      (push (causalt-suffix-test-vector "c" #'causal-dired-change-tmenu) test-vectors)
      (push (causalt-suffix-test-vector "y" #'dired-show-file-type) test-vectors)
      (push (causalt-suffix-test-vector "w" #'dired-copy-filename-as-kill) test-vectors)
      (push (causalt-suffix-test-vector "!" #'dired-do-shell-command) test-vectors)
      (push (causalt-suffix-test-vector "&" #'dired-do-async-shell-command) test-vectors)
      (push (causalt-suffix-test-vector "W" #'browse-url-of-dired-file) test-vectors)

      (push (causalt-suffix-test-vector "s" #'causal-dired-sort-by-tmenu) test-vectors)
      (push (causalt-suffix-test-vector "h" #'dired-hide-details-mode) test-vectors)
      (push (causalt-suffix-test-vector "O" #'dired-omit-mode) test-vectors)
      ;; (push (causalt-suffix-test-vector "iq" #'dired-maybe-insert-subdir 2) test-vectors)
      (push (causalt-suffix-test-vector "$" #'dired-hide-subdir) test-vectors)
      (push (causalt-suffix-test-vector "k" #'dired-do-kill-lines) test-vectors)
      (push (causalt-suffix-test-vector "g" #'revert-buffer) test-vectors)
      (push (causalt-suffix-test-vector "f" #'causal-dired-find-dired-regexp) test-vectors)
      (push (causalt-suffix-test-vector "E" #'wdired-change-to-wdired-mode) test-vectors)
      ;; (push (causalt-suffix-test-vector "T" #'image-dired) test-vectors) ; breaks in command line because not display-graphic-p
      (push (causalt-suffix-test-vector "m" #'dired-mark) test-vectors)
      (push (causalt-suffix-test-vector "u" #'dired-unmark) test-vectors)
      (push (causalt-suffix-test-vector "U" #'dired-unmark-all-marks) test-vectors)
      (push (causalt-suffix-test-vector "t" #'dired-toggle-marks) test-vectors)
      (push (causalt-suffix-test-vector "~" #'dired-flag-backup-files) test-vectors)
      (push (causalt-suffix-test-vector "x" #'dired-do-flagged-delete) test-vectors)
      (push (causalt-suffix-test-vector "r" #'causal-dired-regexp-tmenu) test-vectors)

      (push (causalt-suffix-test-vector "^" #'dired-up-directory) test-vectors)
      (push (causalt-suffix-test-vector "p" #'dired-previous-line) test-vectors)
      (push (causalt-suffix-test-vector "n" #'dired-next-line) test-vectors)
      (push (causalt-suffix-test-vector "ð" #'dired-prev-dirline) test-vectors)
      (push (causalt-suffix-test-vector "î" #'dired-next-dirline) test-vectors)
      (push (causalt-suffix-test-vector "j" #'dired-goto-file) test-vectors)
      (push (causalt-suffix-test-vector "ê" #'dired-goto-subdir) test-vectors)
      ;;(push (causalt-suffix-test-vector "[" #'dired-prev-subdir) test-vectors)
      ;;(push (causalt-suffix-test-vector "]" #'dired-next-subdir) test-vectors)

      (push (causalt-suffix-test-vector "J" #'bookmark-jump) test-vectors)
      (push (causalt-suffix-test-vector "B" #'bookmark-set-no-overwrite) test-vectors)
      (push (causalt-suffix-test-vector "b" #'ibuffer) test-vectors)

      ;;(push (causalt-suffix-test-vector "" dired-isearch-filenames) test-vectors)
      (push (causalt-suffix-test-vector "ó" #'dired-isearch-filenames-regexp) test-vectors)
      (push (causalt-suffix-test-vector "/" #'causal-dired-search-replace-tmenu) test-vectors)

      ;;(push (causalt-suffix-test-vector "+q" #'dired-create-directory) test-vectors)
      (push (causalt-suffix-test-vector "F" #'dired-create-empty-file) test-vectors)
      (push (causalt-suffix-test-vector "æ" #'rgrep) test-vectors)

      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-dired-tmenu
                                       '(lambda () (random 5000)))))
  (causalt-dired-breakdown t))


(ert-deftest test-causal-dired-regexp-tmenu-bindings ()
  (causalt-dired-setup)
  (let ((test-vectors (list)))
    (push (causalt-suffix-test-vector "m" #'dired-mark-files-regexp) test-vectors)
    (push (causalt-suffix-test-vector "c" #'dired-mark-files-containing-regexp) test-vectors)
    (push (causalt-suffix-test-vector "d" #'dired-flag-files-regexp) test-vectors)
    (push (causalt-suffix-test-vector "C" #'dired-do-copy-regexp) test-vectors)
    (push (causalt-suffix-test-vector "r" #'dired-do-rename-regexp) test-vectors)

    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-dired-regexp-tmenu
                                     '(lambda () (random 5000))))
  (causalt-dired-breakdown t))

(ert-deftest test-causal-dired-change-tmenu-bindings ()
  (causalt-dired-setup)
  (let ((test-vectors (list)))
    (push (causalt-suffix-test-vector "M" #'dired-do-chmod) test-vectors)
    (push (causalt-suffix-test-vector "G" #'dired-do-chgrp) test-vectors)
    (push (causalt-suffix-test-vector "O" #'dired-do-chown) test-vectors)
    (push (causalt-suffix-test-vector "T" #'dired-do-touch) test-vectors)

    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-dired-change-tmenu
                                     '(lambda () (random 5000))))
  (causalt-dired-breakdown t))


(ert-deftest test-causal-dired-format-arrow ()
  (should (string-equal (causal-dired-format-arrow "hey" t) " hey"))
  (should (string-equal (causal-dired-format-arrow "hey" nil) "hey")))

(provide 'test-causal-dired)
;;; test-causal-dired.el ends here
