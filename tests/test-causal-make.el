;;; test-causal-make.el --- Causal Make Tests -*- lexical-binding: t; -*-

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
(require 'causal-make-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-make)

(ert-deftest test-causal-make-tmenu ()
  (let ((tmpfile "Makefile"))
    (causalt-make-setup tmpfile)
    (cl-letf ((causalt-mock #'makefile-backslash-region)
              (causalt-mock #'comment-region)
              (causalt-mock #'makefile-insert-target-ref)
              (causalt-mock #'makefile-insert-macro-ref)
              (causalt-mock #'makefile-insert-gmake-function)
              (causalt-mock #'makefile-pickup-everything)
              (causalt-mock #'makefile-pickup-filenames-as-targets)
              (causalt-mock #'makefile-create-up-to-date-overview)
              (causalt-mock #'makefile-previous-dependency)
              (causalt-mock #'makefile-next-dependency)
              (causalt-mock #'compile)
              (causalt-mock #'imenu))

      (let ((test-vectors
             '((:binding "\\" :command makefile-backslash-region)
               (:binding ";" :command comment-region)
               (:binding ":" :command makefile-insert-target-ref)
               (:binding "m" :command makefile-insert-macro-ref)
               (:binding "f" :command makefile-insert-gmake-function)
               (:binding "a" :command causal-make-automatic-variables-tmenu)

               (:binding "E" :command makefile-pickup-everything)
               (:binding "F" :command makefile-pickup-filenames-as-targets)

               (:binding "c" :command compile)
               (:binding "o" :command makefile-create-up-to-date-overview)
               (:binding "t" :command causal-make-mode-select-tmenu)
               (:binding "." :command causal-make-identify-autovar-region)

               ;; (:binding "i" :command imenu)
               (:binding "p" :command makefile-previous-dependency)
               (:binding "n" :command makefile-next-dependency))))

        (causalt-mock-active-region)
        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-make-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-make-breakdown tmpfile)))

(provide 'test-causal-make)
;;; test-causal-make.el ends here
