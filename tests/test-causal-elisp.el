;;; test-causal-elisp.el --- Causal Make Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Y. Choi

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
(require 'causal-elisp-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-elisp)

(ert-deftest test-causal-elisp-tmenu ()
  (let ((tmpfile "scratch-elisp.el"))
    (causalt-elisp-setup)
    (cl-letf (
              (causalt-mock #'eval-last-sexp)
              (causalt-mock #'elisp-eval-region-or-buffer)
              (causalt-mock #'eval-defun)
              (causalt-mock #'xref-find-definitions)
              (causalt-mock #'xref-find-references)
              (causalt-mock #'xref-find-references-and-replace)
              (causalt-mock #'checkdoc)
              (causalt-mock #'elisp-byte-compile-file)
              (causalt-mock #'elisp-byte-compile-buffer)
              (causalt-mock #'byte-recompile-directory)
              (causalt-mock #'find-library)
              (causalt-mock #'find-variable)
              (causalt-mock #'find-function)
              (causalt-mock #'backward-char)
              (causalt-mock #'backward-sexp)
              (causalt-mock #'forward-char)
              (causalt-mock #'causal-elisp-next-sexp)
              (causalt-mock #'previous-line)
              (causalt-mock #'backward-up-list)
              (causalt-mock #'next-line)
              (causalt-mock #'down-list)
              (causalt-mock #'causal-elisp-settings-tmenu)
              (causalt-mock #'transient-quit-all))

      (let ((test-vectors
             '((:binding "x" :command eval-last-sexp)
               (:binding "L" :command elisp-eval-region-or-buffer)
               (:binding "d" :command eval-defun)

               ;;(:binding "." :command xref-find-definitions)
               ;;(:binding "r" :command xref-find-references)
               ;;(:binding "R" :command xref-find-references-and-replace)

               (:binding "c" :command checkdoc)

               (:binding "B" :command elisp-byte-compile-file)
               (:binding "b" :command elisp-byte-compile-buffer)
               (:binding "D" :command byte-recompile-directory)

               (:binding "l" :command find-library)
               (:binding "v" :command find-variable)
               (:binding "f" :command find-function)

               (:binding "<left>" :command backward-char)
               (:binding "C-<left>" :command backward-sexp)
               (:binding "<right>" :command forward-char)
               (:binding "C-<right>" :command causal-elisp-next-sexp)
               (:binding "<up>" :command previous-line)
               (:binding "C-<up>" :command backward-up-list)
               (:binding "<down>" :command next-line)
               (:binding "C-<down>" :command down-list)
               (:binding "RET" :command transient-quit-all))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-elisp-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-elisp-breakdown)))

(provide 'test-causal-elisp)
;;; test-causal-elisp.el ends here
