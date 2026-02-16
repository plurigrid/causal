;;; test-causal-info.el --- Causal Info Tests      -*- lexical-binding: t; -*-

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
(require 'causal-info-test-utils)
(require 'causal-info)

(ert-deftest test-causal-info-tmenu ()
  (let ((tmpfile "causal-info-tmenu.txt"))
    (causalt-info-setup)
    (cl-letf (
              (causalt-mock #'Info-directory)
              (causalt-mock #'info-display-manual)
              (causalt-mock #'Info-top-node)
              (causalt-mock #'Info-toc)
              (causalt-mock #'Info-menu)
              (causalt-mock #'Info-goto-node)
              (causalt-mock #'Info-index)
              (causalt-mock #'Info-virtual-index)
              (causalt-mock #'Info-history)
              (causalt-mock #'Info-history-back)
              (causalt-mock #'Info-history-forward)
              (causalt-mock #'Info-search)
              (causalt-mock #'isearch-forward)
              (causalt-mock #'Info-search-case-sensitively)
              (causalt-mock #'Info-scroll-up)
              (causalt-mock #'Info-scroll-down)
              (causalt-mock #'Info-prev-reference)
              (causalt-mock #'Info-next-reference)
              (causalt-mock #'Info-backward-node)
              (causalt-mock #'Info-forward-node)
              (causalt-mock #'Info-prev)
              (causalt-mock #'Info-next)
              (causalt-mock #'Info-top-node)
              (causalt-mock #'Info-final-node)
              (causalt-mock #'Info-follow-nearest-node)
              (causalt-mock #'Info-up)
              (causalt-mock #'Info-copy-current-node-name)
              (causalt-mock #'Info-goto-node-web)
              (causalt-mock #'clone-buffer)
              (causalt-mock #'bookmark-jump)
              (causalt-mock #'bookmark-set)
              (causalt-mock #'ibuffer)
              (causalt-mock #'quit-window)
              (causalt-mock #'info-apropos))

      (let ((test-vectors
             '(;; Overview
               (:binding "d" :command Info-directory)
               (:binding "M" :command info-display-manual)
               (:binding "t" :command Info-top-node)
               (:binding "T" :command Info-toc)

               ;; Goto
               (:binding "m" :command Info-menu)
               ;;(:binding "g" :command Info-goto-node)
               (:binding "i" :command Info-index)
               (:binding "I" :command Info-virtual-index)

               ;; Search
               (:binding "C-s" :command isearch-forward)
               (:binding "s" :command Info-search)
               (:binding "S" :command Info-search-case-sensitively)
               (:binding "a" :command info-apropos)

               ;; History
               (:binding "L" :command Info-history)
               (:binding "M-[" :command Info-history-back)
               (:binding "M-]" :command Info-history-forward)

               ;; Scroll
               ;;(:binding "S-SPC" :command Info-scroll-down)
               (:binding "SPC" :command Info-scroll-up)
               (:binding "DEL" :command Info-scroll-down)

               ;; Navigation
               (:binding "k" :command Info-prev-reference)
               (:binding "j" :command Info-next-reference)
               (:binding "p" :command causal-lib-browse-backward-paragraph)
               (:binding "n" :command causal-lib-browse-forward-paragraph)
               (:binding "[" :command Info-backward-node)
               (:binding "]" :command Info-forward-node)
               (:binding "h" :command Info-prev)
               (:binding "l" :command Info-next)
               (:binding "<" :command Info-top-node)
               (:binding ">" :command Info-final-node)
               (:binding "RET" :command Info-follow-nearest-node)
               (:binding "^" :command Info-up)

               ;; Quick
               (:binding "c" :command Info-copy-current-node-name)
               (:binding "M-n" :command clone-buffer)
               (:binding "C-M-n" :command causal-info-new-info-frame)
               (:binding "G" :command Info-goto-node-web)

               ;; Menu Navigation
               (:binding "," :command causal-info-settings-tmenu)
               (:binding "q" :command quit-window))))

        (info "Emacs")
        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-info-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-info-breakdown t)))

(provide 'test-causal-info)
;;; test-causal-info.el ends here
