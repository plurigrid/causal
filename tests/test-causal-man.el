;;; test-causal-man.el --- Causal Make Tests -*- lexical-binding: t; -*-

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
(require 'causal-man-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-man)

(ert-deftest test-causal-man-tmenu ()
  (let ()
    (causalt-man-setup)

    (cl-letf ((causalt-mock #'beginning-of-buffer)
              (causalt-mock #'end-of-buffer)
              (causalt-mock #'previous-line)
              (causalt-mock #'next-line)
              (causalt-mock #'causal-lib-browse-backward-paragraph)
              (causalt-mock #'causal-lib-browse-forward-paragraph)
              (causalt-mock #'causal-man-occur-options)
              (causalt-mock #'Man-previous-section)
              (causalt-mock #'Man-next-section)
              (causalt-mock #'Man-goto-section)
              (causalt-mock #'Man-goto-see-also-section)
              (causalt-mock #'Man-follow-manual-reference)
              (causalt-mock #'Man-previous-manpage)
              (causalt-mock #'Man-next-manpage)
              (causalt-mock #'Man-goto-page)
              (causalt-mock #'bookmark-set-no-overwrite)
              (causalt-mock #'bookmark-jump)
              (causalt-mock #'causal-man-info)
              (causalt-mock #'man)
              (causalt-mock #'Man-update-manpage)
              (causalt-mock #'Man-kill)
              (causalt-mock #'quit-window))

      (let ((test-vectors
             '((:binding "." :command beginning-of-buffer)
               (:binding ">" :command end-of-buffer)
               (:binding "C-n" :command next-line)
               (:binding "C-p" :command previous-line)
               (:binding "n" :command causal-lib-browse-forward-paragraph)
               (:binding "p" :command causal-lib-browse-backward-paragraph)
               (:binding "o" :command causal-man-occur-options)
               (:binding "]" :command Man-next-section)
               (:binding "[" :command Man-previous-section)
               (:binding "g" :command Man-goto-section)
               (:binding "s" :command Man-goto-see-also-section)
              ;; (:binding "r" :command Man-follow-manual-reference)
              ;; (:binding "M-n" :command Man-next-manpage)
              ;; (:binding "M-p" :command Man-previous-manpage)
              ;; (:binding "j" :command Man-goto-page)
               (:binding "B" :command bookmark-set-no-overwrite)
              ;; (:binding "J" :command bookmark-jump)
               ;; (:binding "m" :command man)
               (:binding "I" :command causal-man-info)
               (:binding "u" :command Man-update-manpage)
               (:binding "," :command causal-man-settings-tmenu)
               (:binding "K" :command Man-kill)
               (:binding "q" :command quit-window)
               )))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-man-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-man-breakdown)))

(provide 'test-causal-man)
;;; test-causal-man.el ends here
