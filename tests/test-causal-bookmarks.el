;;; test-causal-bookmarks.el --- Causal IBuffer Tests -*- lexical-binding: t; -*-

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
(require 'causal-bookmarks-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-bookmarks)

(ert-deftest test-causal-bookmarks-tmenu-bindings ()
  (causalt-bookmarks-setup)
  (let
      ((test-vectors
        (list
         (causalt-suffix-test-vector "" #'bookmark-bmenu-this-window)
         (causalt-suffix-test-vector "o" #'bookmark-bmenu-other-window)
         (causalt-suffix-test-vector "v" #'bookmark-bmenu-select)
         (causalt-suffix-test-vector "r" #'bookmark-bmenu-rename)
         (causalt-suffix-test-vector "R" #'bookmark-bmenu-relocate)
         (causalt-suffix-test-vector "x" #'bookmark-bmenu-execute-deletions)
         (causalt-suffix-test-vector "+" #'causal-bookmarks-add-bookmark-via-buffer)
         (causalt-suffix-test-vector "w" #'bookmark-bmenu-locate)

         (causalt-suffix-test-vector "m" #'bookmark-bmenu-mark)
         (causalt-suffix-test-vector "M" #'bookmark-bmenu-mark-all)
         (causalt-suffix-test-vector "u" #'bookmark-bmenu-unmark)
         (causalt-suffix-test-vector "U" #'bookmark-bmenu-unmark-all)
         (causalt-suffix-test-vector "d" #'bookmark-bmenu-delete)
         (causalt-suffix-test-vector "D" #'bookmark-bmenu-delete-all)

         (causalt-suffix-test-vector "S" #'causal-bookmarks-sortby-tmenu)
         (causalt-suffix-test-vector "t" #'bookmark-bmenu-toggle-filenames)
         (causalt-suffix-test-vector "/" #'bookmark-bmenu-search)
         (causalt-suffix-test-vector "g" #'revert-buffer)

         (causalt-suffix-test-vector "e" #'bookmark-bmenu-edit-annotation)
         (causalt-suffix-test-vector "a" #'bookmark-bmenu-show-annotation)
         (causalt-suffix-test-vector "A" #'bookmark-bmenu-show-all-annotations)

         (causalt-suffix-test-vector "p" #'previous-line)
         (causalt-suffix-test-vector "n" #'next-line)
         (causalt-suffix-test-vector "<" #'beginning-of-buffer)
         (causalt-suffix-test-vector ">" #'end-of-buffer)
         (causalt-suffix-test-vector "J" #'bookmark-jump)

         (causalt-suffix-test-vector "b" #'tabulated-list-previous-column)
         (causalt-suffix-test-vector "f" #'tabulated-list-next-column)
         (causalt-suffix-test-vector "{" #'tabulated-list-narrow-current-column)
         (causalt-suffix-test-vector "}" #'tabulated-list-widen-current-column)

         (causalt-suffix-test-vector "s" #'bookmark-bmenu-save)
         (causalt-suffix-test-vector "," #'causal-bookmarks-settings-tmenu)
         (causalt-suffix-test-vector "q" #'quit-window))))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-bookmarks-tmenu
                                     '(lambda () (random 5000))))
  (causalt-bookmarks-breakdown t))

(provide 'test-causal-bookmarks)
;;; test-causal-bookmarks.el ends here
