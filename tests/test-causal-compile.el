;;; test-causal-compile.el --- Causal Make Tests -*- lexical-binding: t; -*-

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
(require 'causal-compile-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-compile)

;; TODO: Need to figure out a way to deal with exiting out of grep.
;; (ert-deftest test-causal-compile-tmenu ()
;;   (let ((tempfile "foo"))
;;     ;;(causalt-compile-setup tempfile)

;;     (cl-letf ((causalt-mock #'previous-error-no-select)
;;               (causalt-mock #'next-error-no-select)
;;               (causalt-mock #'compilation-previous-error)
;;               (causalt-mock #'compilation-next-error)
;;               (causalt-mock #'compilation-display-error)
;;               (causalt-mock #'compile-goto-error)
;;               (causalt-mock #'compilation-previous-file)
;;               (causalt-mock #'compilation-next-file)
;;               (causalt-mock #'compile)
;;               (causalt-mock #'recompile)
;;               (causalt-mock #'kill-compilation)
;;               (causalt-mock #'causal-compile-settings-tmenu)
;;               (causalt-mock #'quit-window))

;;       (let ((test-vectors
;;              '((:binding "n" :command next-error-no-select)
;;                (:binding "p" :command previous-error-no-select)
;;                (:binding "j" :command compilation-next-error)
;;                (:binding "k" :command compilation-previous-error)
;;                (:binding "o" :command compilation-display-error)
;;                (:binding "RET" :command compile-goto-error)
;;                (:binding "[" :command compilation-previous-file)
;;                (:binding "]" :command compilation-next-file)
;;                ;; (:binding "c" :command compile)
;;                ;; (:binding "g" :command recompile)
;;                ;; (:binding "k" :command kill-compilation)
;;                (:binding "," :command causal-compile-settings-tmenu)
;;                (:binding "q" :command quit-window))
;;              ))

;;         (causalt-suffix-testcase-runner test-vectors
;;                                         #'causal-compile-tmenu
;;                                         '(lambda () (random 5000)))))
;;     ;; (causalt-compile-breakdown tempfile)
;;     ))

(provide 'test-causal-compile)
;;; test-causal-compile.el ends here
