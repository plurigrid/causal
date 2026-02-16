;;; test-causal-eww.el --- Causal Make Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Charles Y. Choi

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
(require 'causal-eww-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-eww)

(ert-deftest test-causal-eww-tmenu ()
  (let ()
    (causalt-eww-setup)

    (cl-letf (
              (causalt-mock #'eww-back-url)
              (causalt-mock #'eww-forward-url)
              (causalt-mock #'eww-list-histories)
              (causalt-mock #'eww-previous-url)
              (causalt-mock #'eww-next-url)
              (causalt-mock #'eww-up-url)
              (causalt-mock #'eww-top-url)
              (causalt-mock #'causal-lib-browse-backward-paragraph)
              (causalt-mock #'causal-lib-browse-forward-paragraph)
              (causalt-mock #'scroll-up-command)
              (causalt-mock #'scroll-down-command)
              (causalt-mock #'shr-previous-link)
              (causalt-mock #'shr-next-link)
              (causalt-mock #'eww-follow-link)
              (causalt-mock #'causal-eww-display-tmenu)
              (causalt-mock #'eww-readable)
              (causalt-mock #'eww)
              (causalt-mock #'eww-browse-with-external-browser)
              (causalt-mock #'eww-copy-page-url)
              (causalt-mock #'eww-copy-alternate-url)
              (causalt-mock #'eww-download)
              (causalt-mock #'eww-reload)
              (causalt-mock #'eww-add-bookmark)
              (causalt-mock #'eww-list-bookmarks)
              (causalt-mock #'eww-next-bookmark)
              (causalt-mock #'eww-previous-bookmark)
              (causalt-mock #'causal-eww-settings-tmenu)
              (causalt-mock #'causal-eww-info)
              (causalt-mock #'quit-window))

      (let ((test-vectors
             '((:binding "M-[" :command eww-back-url)
               (:binding "M-]" :command eww-forward-url)
               (:binding "H" :command eww-list-histories)
               (:binding "[" :command eww-previous-url)
               (:binding "]" :command eww-next-url)
               (:binding "^" :command eww-up-url)
               (:binding "t" :command eww-top-url)
               (:binding "p" :command causal-lib-browse-backward-paragraph)
               (:binding "n" :command causal-lib-browse-forward-paragraph)
               (:binding "SPC" :command scroll-up-command)
               (:binding "S-SPC" :command scroll-down-command)
               (:binding "k" :command shr-previous-link)
               (:binding "j" :command shr-next-link)
               (:binding "RET" :command eww-follow-link)
               (:binding "D" :command causal-eww-display-tmenu)
               (:binding "R" :command eww-readable)
               (:binding "M-l" :command eww)
               (:binding "&" :command eww-browse-with-external-browser)
               (:binding "c" :command eww-copy-page-url)
               (:binding "A" :command eww-copy-alternate-url)
               (:binding "d" :command eww-download)
               (:binding "g" :command eww-reload)
               (:binding "b" :command eww-add-bookmark)
               (:binding "B" :command eww-list-bookmarks)
               (:binding "M-n" :command eww-next-bookmark)
               (:binding "M-p" :command eww-previous-bookmark)
               (:binding "," :command causal-eww-settings-tmenu)
               (:binding "I" :command causal-eww-info)
               (:binding "q" :command quit-window))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-eww-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-eww-breakdown)))


(ert-deftest test-causal-eww-bookmarks-tmenu ()
  (let ((tmpfile "causal-eww-bookmarks-tmenu.txt"))
    (causalt-eww-setup)
    (cl-letf ((causalt-mock #'eww-bookmark-kill)
              (causalt-mock #'eww-bookmark-yank)
              (causalt-mock #'eww-bookmark-browse)
              (causalt-mock #'previous-line)
              (causalt-mock #'next-line))

      (let ((test-vectors
             '((:binding "k" :command eww-bookmark-kill)
               (:binding "y" :command eww-bookmark-yank)
               (:binding "RET" :command eww-bookmark-browse)
               (:binding "p" :command previous-line)
               (:binding "n" :command next-line))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-eww-bookmarks-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-eww-breakdown)))

(provide 'test-causal-eww)
;;; test-causal-eww.el ends here
