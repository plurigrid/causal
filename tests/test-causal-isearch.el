;;; test-causal-isearch.el --- Causal Re-Builder Tests -*- lexical-binding: t; -*-

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
(require 'causal-isearch-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-isearch)

(ert-deftest test-causal-isearch-tmenu ()
  (let ((tmpfile "causal-isearch-tmenu.txt"))
    (causalt-setup)
    (cl-letf (;;((symbol-function #') (lambda () t))
              (causalt-mock #'isearch-edit-string)
              (causalt-mock #'isearch-yank-word-or-char)
              (causalt-mock #'isearch-yank-symbol-or-char)
              (causalt-mock #'isearch-yank-line)
              (causalt-mock #'isearch-yank-kill)
              (causalt-mock #'isearch-forward-thing-at-point)
              (causalt-mock #'isearch-query-replace)
              (causalt-mock #'isearch-query-replace-regexp)
              (causalt-mock #'isearch-toggle-regexp)
              (causalt-mock #'isearch-toggle-symbol)
              (causalt-mock #'isearch-toggle-word)
              (causalt-mock #'isearch-toggle-case-fold)
              (causalt-mock #'isearch-toggle-lax-whitespace)
              (causalt-mock #'isearch-occur)
              (causalt-mock #'isearch-highlight-regexp)
              (causalt-mock #'isearch-highlight-lines-matching-regexp)
              (causalt-mock #'isearch-repeat-forward)
              (causalt-mock #'isearch-repeat-backward))

      (let ((test-vectors
             '((:binding "e" :command isearch-edit-string)
               (:binding "w" :command isearch-yank-word-or-char)
               (:binding "s" :command isearch-yank-symbol-or-char)
               (:binding "l" :command isearch-yank-line)
               (:binding "y" :command isearch-yank-kill)
               (:binding "t" :command isearch-forward-thing-at-point)

               (:binding "r" :command isearch-query-replace)
               (:binding "x" :command isearch-query-replace-regexp)

               (:binding "X" :command isearch-toggle-regexp)
               (:binding "S" :command isearch-toggle-symbol)
               (:binding "W" :command isearch-toggle-word)
               (:binding "F" :command isearch-toggle-case-fold)
               (:binding "L" :command isearch-toggle-lax-whitespace)
               (:binding "o" :command isearch-occur)
               (:binding "h" :command isearch-highlight-regexp)
               (:binding "H" :command isearch-highlight-lines-matching-regexp)
               (:binding "n" :command isearch-repeat-forward)
               (:binding "p" :command isearch-repeat-backward)
               (:binding ">" :command isearch-end-of-buffer)
               (:binding "<" :command isearch-beginning-of-buffer)
               (:binding "RET" :command isearch-exit))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-isearch-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-breakdown)))



(provide 'test-causal-isearch)
;;; test-causal-isearch.el ends here
