;;; test-causal-help.el --- Causal Make Tests -*- lexical-binding: t; -*-

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
(require 'causal-lib-test-utils)
(require 'causal-help-test-utils)
(require 'causal-help)

(ert-deftest test-causal-help-tmenu ()
  (let ()
    (causalt-help-setup)

    (cl-letf ((causalt-mock #'beginning-of-buffer)
              (causalt-mock #'end-of-buffer)
              (causalt-mock #'previous-line)
              (causalt-mock #'next-line)
              (causalt-mock #'causal-lib-browse-backward-paragraph)
              (causalt-mock #'causal-lib-browse-forward-paragraph)
              (causalt-mock #'help-goto-previous-page)
              (causalt-mock #'help-goto-next-page)
              (causalt-mock #'help-go-back)
              (causalt-mock #'help-go-forward)
              (causalt-mock #'forward-button)
              (causalt-mock #'backward-button)
              (causalt-mock #'push-button)
              (causalt-mock #'describe-symbol)
              (causalt-mock #'describe-command)
              (causalt-mock #'describe-function)
              (causalt-mock #'describe-variable)

              (causalt-mock #'help-goto-info)
              (causalt-mock #'help-goto-lispref-info)

              (causalt-mock #'help-view-source)
              (causalt-mock #'help-customize)
              (causalt-mock #'causal-help-settings-tmenu)

              (causalt-mock #'quit-window))

      (let ((test-vectors
             '((:binding ">" :command end-of-buffer)
               (:binding "<" :command beginning-of-buffer)
               (:binding "C-n" :command next-line)
               (:binding "C-p" :command previous-line)
               (:binding "n" :command causal-lib-browse-forward-paragraph)
               (:binding "p" :command causal-lib-browse-backward-paragraph)
               (:binding "P" :command help-goto-previous-page)
               (:binding "N" :command help-goto-next-page)
               (:binding "M-[" :command help-go-back)
               (:binding "M-]" :command help-go-forward)

               (:binding "j" :command forward-button)
               (:binding "k" :command backward-button)
               (:binding "RET" :command push-button)

               ;; (:binding "ds" :command describe-symbol)
               ;; (:binding "dv" :command describe-variable)
               ;; (:binding "dc" :command describe-command)
               ;; (:binding "df" :command describe-function)

               (:binding "i" :command help-goto-info)
               (:binding "I" :command help-goto-lispref-info)

               (:binding "s" :command help-view-source)
               ;;(:binding "c" :command help-customize)

               (:binding "," :command causal-help-settings-tmenu)
               (:binding "q" :command quit-window)
               )))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-help-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-help-breakdown)))

(provide 'test-causal-help)
;;; test-causal-help.el ends here
