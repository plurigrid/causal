;;; test-causal-html.el --- Causal HTML Tests -*- lexical-binding: t; -*-

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
(require 'causal-html-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-html)

(ert-deftest test-causal-html-tmenu ()
  (let ((auto-insert-mode -1)
        (temp-file-name "causal-html-test.html"))
    (causalt-html-setup temp-file-name)

    (cl-letf ((causalt-mock #'sgml-tag)
              (causalt-mock #'sgml-attributes)
              (causalt-mock #'sgml-close-tag)
              (causalt-mock #'sgml-delete-tag)
              (causalt-mock #'causal-html-tags-tmenu)
              (causalt-mock #'sgml-skip-tag-backward)
              (causalt-mock #'sgml-skip-tag-forward)
              (causalt-mock #'backward-char)
              (causalt-mock #'forward-char)
              (causalt-mock #'next-line)
              (causalt-mock #'previous-line)
              (causalt-mock #'browse-url-of-buffer)
              (causalt-mock #'sgml-validate)
              (causalt-mock #'html-autoview-mode)
              (causalt-mock #'causal-html-settings-tmenu)
              (causalt-mock #'causal-html-info))

      (let ((test-vectors
             '((:binding "i" :command sgml-tag)
               (:binding "a" :command sgml-attributes)
               (:binding "c" :command sgml-close-tag)
               ;;(:binding "d" :command sgml-delete-tag) ; need to handle html-ts-mode
               (:binding "h" :command causal-html-tags-tmenu)
               (:binding "b" :command browse-url-of-buffer)
               (:binding "v" :command sgml-validate)
               (:binding "A" :command html-autoview-mode)
               (:binding "h" :command causal-html-tags-tmenu)
               (:binding "[" :command sgml-skip-tag-backward)
               (:binding "]" :command sgml-skip-tag-forward)
               (:binding "C-f" :command forward-char)
               (:binding "C-b" :command backward-char)
               (:binding "C-n" :command next-line)
               (:binding "C-p" :command previous-line)
               (:binding "I" :command causal-html-info)
               (:binding "," :command causal-html-settings-tmenu)
               (:binding "TAB" :command sgml-tags-invisible))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-html-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-html-breakdown temp-file-name)))

(ert-deftest test-causal-html-tags-tmenu ()
  (let ((auto-insert-mode -1)
        (temp-file-name "causal-html-tags-test.html"))
    (causalt-html-setup temp-file-name)

    (cl-letf ((causalt-mock #'html-paragraph)
              (causalt-mock #'html-image)
              (causalt-mock #'html-line)
              (causalt-mock #'html-horizontal-rule)
              (causalt-mock #'html-div)
              (causalt-mock #'html-href-anchor)
              (causalt-mock #'html-href-anchor-file)
              (causalt-mock #'html-id-anchor)
              (causalt-mock #'html-name-anchor)
              (causalt-mock #'html-ordered-list)
              (causalt-mock #'html-unordered-list)
              (causalt-mock #'html-list-item)
              (causalt-mock #'html-radio-buttons)
              (causalt-mock #'html-checkboxes)
              (causalt-mock #'html-headline-1)
              (causalt-mock #'html-headline-2)
              (causalt-mock #'html-headline-3)
              (causalt-mock #'html-headline-4)
              (causalt-mock #'html-headline-5)
              (causalt-mock #'html-headline-6)

              (causalt-mock #'facemenu-set-bold)
              (causalt-mock #'facemenu-set-italic)
              (causalt-mock #'facemenu-set-underline)
              (causalt-mock #'facemenu-set-bold-italic)
              (causalt-mock #'facemenu-set-default)
              (causalt-mock #'facemenu-set-face)

              (causalt-mock #'sgml-skip-tag-backward)
              (causalt-mock #'sgml-skip-tag-forward)
              (causalt-mock #'backward-char)
              (causalt-mock #'forward-char)
              (causalt-mock #'next-line)
              (causalt-mock #'previous-line)

              )

      (let ((test-vectors
             '(
               (:binding "p" :command html-paragraph)
               (:binding "i" :command html-image)
               (:binding "b" :command html-line)
               (:binding "hr" :command html-horizontal-rule)
               (:binding "d" :command html-div)
               (:binding "aa" :command html-href-anchor)
               (:binding "af" :command html-href-anchor-file)
               (:binding "ai" :command html-id-anchor)
               (:binding "an" :command html-name-anchor)
               (:binding "o" :command html-ordered-list)
               (:binding "u" :command html-unordered-list)
               (:binding "l" :command html-list-item)
               (:binding "r" :command html-radio-buttons)
               (:binding "c" :command html-checkboxes)
               ;; (:binding "1" :command html-headline-1)
               ;; (:binding "2" :command html-headline-2)
               ;; (:binding "3" :command html-headline-3)
               ;; (:binding "4" :command html-headline-4)
               ;; (:binding "5" :command html-headline-5)
               ;; (:binding "6" :command html-headline-6)

               (:binding "fb" :command facemenu-set-bold)
               (:binding "fi" :command facemenu-set-italic)
               (:binding "fu" :command facemenu-set-underline)
               (:binding "fl" :command facemenu-set-bold-italic)
               (:binding "fd" :command facemenu-set-default)
               (:binding "ff" :command facemenu-set-face)

               (:binding "[" :command sgml-skip-tag-backward)
               (:binding "]" :command sgml-skip-tag-forward)
               (:binding "C-f" :command forward-char)
               (:binding "C-b" :command backward-char)
               (:binding "C-n" :command next-line)
               (:binding "C-p" :command previous-line))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-html-tags-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-html-breakdown temp-file-name)))

(provide 'test-causal-html)
;;; test-causal-html.el ends here
