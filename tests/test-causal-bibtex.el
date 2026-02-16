;;; test-causal-bibtex.el --- BibTeX Tests           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

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
(require 'causal-bibtex-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-bibtex)

(ert-deftest test-causal-bibtex-tmenu ()
  (let ((tmpfile "causal-bibtex-main-tmenu.txt"))
    (causalt-bibtex-setup)
    (cl-letf ((causalt-mock #'bibtex-make-field)
              (causalt-mock #'bibtex-copy-field-as-kill)
              (causalt-mock #'causal-bibtex-copy-field-value)
              (causalt-mock #'bibtex-empty-field)
              (causalt-mock #'bibtex-kill-field)
              (causalt-mock #'bibtex-remove-OPT-or-ALT)
              (causalt-mock #'causal-bibtex-beginning-of-field)
              (causalt-mock #'causal-bibtex-end-of-field)
              (causalt-mock #'previous-line)
              (causalt-mock #'bibtex-next-field)
              (causalt-mock #'bibtex-entry)
              (causalt-mock #'bibtex-copy-entry-as-kill)
              (causalt-mock #'bibtex-kill-entry)
              (causalt-mock #'bibtex-entry-update)
              (causalt-mock #'bibtex-mark-entry)
              (causalt-mock #'bibtex-fill-entry)
              (causalt-mock #'bibtex-clean-entry)
              (causalt-mock #'bibtex-beginning-of-entry)
              (causalt-mock #'bibtex-end-of-entry)
              (causalt-mock #'bibtex-previous-entry)
              (causalt-mock #'bibtex-next-entry)
              (causalt-mock #'bibtex-yank)
              (causalt-mock #'bibtex-yank-pop)
              (causalt-mock #'bibtex-search-entries)
              (causalt-mock #'bibtex-search-crossref)
              (causalt-mock #'bibtex-sort-buffer)
              (causalt-mock #'bibtex-narrow-to-entry)
              (causalt-mock #'occur)
              (causalt-mock #'widen)
              (causalt-mock #'save-buffer)
              (causalt-mock #'bookmark-jump)
              )

      (let ((test-vectors
             '(
               ;; (:binding "a" :command bibtex-make-field)
               (:binding "c" :command bibtex-copy-field-as-kill)
               (:binding "w" :command causal-bibtex-copy-field-value)
               (:binding "x" :command bibtex-empty-field)
               (:binding "DEL" :command bibtex-kill-field)
               (:binding "o" :command bibtex-remove-OPT-or-ALT)
               (:binding "C-a" :command causal-bibtex-beginning-of-field)
               (:binding "C-e" :command causal-bibtex-end-of-field)
               (:binding "p" :command previous-line)
               (:binding "n" :command bibtex-next-field)

               (:binding "A" :command bibtex-entry)
               (:binding "C" :command bibtex-copy-entry-as-kill)
               (:binding "k" :command bibtex-kill-entry)
               (:binding "u" :command bibtex-entry-update)
               (:binding "m" :command bibtex-mark-entry)
               (:binding "f" :command bibtex-fill-entry)
               (:binding "C-c" :command bibtex-clean-entry)
               (:binding "<" :command bibtex-beginning-of-entry)
               (:binding ">" :command bibtex-end-of-entry)
               (:binding "M-p" :command bibtex-previous-entry)
               (:binding "M-n" :command bibtex-next-entry)

               (:binding "y" :command bibtex-yank)
               (:binding "M-y" :command bibtex-yank-pop)
               (:binding "/" :command bibtex-search-entries)
               (:binding "j" :command bibtex-search-entry)
               ;; (:binding "." :command bibtex-search-crossref)
               (:binding "s" :command bibtex-sort-buffer)
               (:binding "O" :command occur)
               (:binding "N" :command bibtex-narrow-to-entry)
               (:binding "C-s" :command save-buffer)
               (:binding "J" :command bookmark-jump)
               )))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-bibtex-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-bibtex-breakdown)))


(provide 'test-causal-bibtex)
;;; test-causal-bibtex.el ends here
