;;; test-causal-eshell.el --- Causal Make Tests -*- lexical-binding: t; -*-

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
(require 'causal-eshell)
(require 'causal-lib-test-utils)
(require 'causal-eshell-test-utils)

(ert-deftest test-causal-eshell-tmenu ()
  (let ()
    (causalt-eshell-setup)

    (cl-letf ((causalt-mock #'eshell-insert-buffer-name)
              (causalt-mock #'eshell-kill-input)
              (causalt-mock #'eshell-list-history)
              (causalt-mock #'eshell-backward-argument)
              (causalt-mock #'eshell-forward-argument)
              (causalt-mock #'eshell-repeat-argument)
              (causalt-mock #'eshell-previous-prompt)
              (causalt-mock #'eshell-next-prompt)
              (causalt-mock #'eshell-copy-old-input)
              (causalt-mock #'eshell-show-output)
              (causalt-mock #'eshell-show-maximum-output)
              (causalt-mock #'eshell-mark-output)
              (causalt-mock #'eshell-delete-output)
              ;;(causalt-mock #'dired)
              (causalt-mock #'causal-eshell-edit-aliases)

              (causalt-mock #'causal-eshell-info-tmenu)
              (causalt-mock #'causal-eshell-settings-tmenu)
              ;;(causalt-mock #'bookmark-jump)
              ;;(causalt-mock #'magit-status)
              )

      (let ((test-vectors
             '((:binding "B" :command eshell-insert-buffer-name)
               (:binding "k" :command eshell-kill-input)
               (:binding "h" :command eshell-list-history)
               (:binding "b" :command eshell-backward-argument)
               (:binding "f" :command eshell-forward-argument)
               (:binding "y" :command eshell-repeat-argument)
               (:binding "p" :command eshell-previous-prompt)
               (:binding "n" :command eshell-next-prompt)
               (:binding "RET" :command eshell-copy-old-input)
               (:binding "s" :command eshell-show-output)
               (:binding "." :command eshell-show-maximum-output)
               (:binding "m" :command eshell-mark-output)
               (:binding "D" :command eshell-delete-output)
               ;; (:binding "d" :command dired)
               (:binding "a" :command causal-eshell-edit-aliases)
               (:binding "i" :command causal-eshell-info-tmenu)
               (:binding "," :command causal-eshell-settings-tmenu))
             ))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-eshell-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-eshell-breakdown)))


(ert-deftest test-causal-eshell-info-tmenu ()
  (let ()
    (causalt-eshell-setup)
    (cl-letf ((causalt-mock #'causal-eshell-info)
              (causalt-mock #'causal-eshell-info-builtins)
              (causalt-mock #'causal-eshell-info-aliases)
              (causalt-mock #'causal-eshell-info-remote-access)
              (causalt-mock #'causal-eshell-info-control-flow)
              (causalt-mock #'causal-eshell-info-expansion)
              (causalt-mock #'causal-eshell-info-dollars-expansion)
              (causalt-mock #'causal-eshell-info-redirection)
              (causalt-mock #'causal-eshell-info-pipelines))

      (let ((test-vectors
             '(
               (:binding "i" :command causal-eshell-info)
               (:binding "b" :command causal-eshell-info-builtins)
               (:binding "a" :command causal-eshell-info-aliases)
               (:binding "r" :command causal-eshell-info-remote-access)
               (:binding "c" :command causal-eshell-info-control-flow)
               (:binding "e" :command causal-eshell-info-expansion)
               (:binding "d" :command causal-eshell-info-dollars-expansion)
               (:binding "R" :command causal-eshell-info-redirection)
               (:binding "p" :command causal-eshell-info-pipelines)
               )))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-eshell-info-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-eshell-breakdown)))

(provide 'test-causal-eshell)
;;; test-causal-eshell.el ends here
