;;; test-causal-editkit.el --- Causal IBuffer Tests -*- lexical-binding: t; -*-

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
(require 'causal-editkit-test-utils)
(require 'causal-editkit)

(ert-deftest test-causal-editkit-tmenu ()
  (let ((tmpfile "causal-editkit-main-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf (((symbol-function #'causal-editkit-version-controlled-p) (lambda () t))
              ((symbol-function #'causal-editkit-package-symbol-overlay-installed-p) (lambda () t))
              ((symbol-function #'causal-editkit-package-magit-installed-p) (lambda () t))
              ((symbol-function #'display-graphic-p) (lambda (&optional d) t))
              (causalt-mock #'find-file-at-point)
              (causalt-mock #'dired-jump-other-window)
              (causalt-mock #'ibuffer)
              (causalt-mock #'recentf-open-files)
              (causalt-mock #'revert-buffer)
              (causalt-mock #'save-buffer)
              (causalt-mock #'write-file)
              (causalt-mock #'widen)
              (causalt-mock #'write-region)

              (causalt-mock #'insert-char)
              (causalt-mock #'fill-paragraph)
              (causalt-mock #'newline)
              (causalt-mock #'join-line)
              (causalt-mock #'mark-sexp)
              (causalt-mock #'kill-sexp)
              (causalt-mock #'transpose-sexp)

              (causalt-mock #'org-agenda)
              (causalt-mock #'compile)
              (causalt-mock #'quick-calc)
              (causalt-mock #'shell-command)
              (causalt-mock #'text-scale-adjust)

              (causalt-mock #'make-frame-command)
              (causalt-mock #'undo)
              (causalt-mock #'save-buffers-kill-emacs))

      (let ((test-vectors
             '((:binding "o" :command causal-editkit-open-tmenu)
               (:binding "f" :command find-file-at-point)
               (:binding "d" :command dired-jump-other-window)
               (:binding "b" :command ibuffer)
               (:binding "R" :command recentf-open-files)
               (:binding "v" :command revert-buffer)
               (:binding "s" :command save-buffer)
               (:binding "S" :command write-file)
               (:binding "y" :command write-region)

               (:binding "e" :command causal-editkit-edit-tmenu)
               (:binding "p" :command fill-paragraph)
               (:binding "l" :command join-line)
               (:binding "C-o" :command open-line)
               (:binding "N" :command causal-editkit-narrow-tmenu)
               ;; (:binding "W" :command widen)

               (:binding "E" :command causal-editkit-emoji-symbols-tmenu)

               (:binding "m" :command mark-sexp)
               (:binding "c" :command causal-editkit-copy-sexp)
               (:binding "k" :command kill-sexp)
               (:binding "t" :command transpose-sexps)

               (:binding "T" :command causal-editkit-tools-tmenu)
               (:binding "a" :command org-agenda)
               (:binding "C" :command compile)
               (:binding "*" :command quick-calc)
               (:binding "!" :command shell-command)
               (:binding "g" :command causal-editkit-select-magit-command)
               (:binding "h" :command causal-editkit-symbol-overlay-put)

               (:binding "B" :command causal-editkit-bookmarks-tmenu)
               (:binding "J" :command bookmark-jump)

               (:binding "w" :command causal-editkit-windows-tmenu)
               (:binding "M-n" :command make-frame-command)

               (:binding "P" :command causal-editkit-project-tmenu)
               (:binding "/" :command causal-editkit-search-tmenu)
               (:binding "M" :command causal-editkit-macro-tmenu)
               (:binding "F" :command text-scale-adjust)

               (:binding "r" :command causal-editkit-registers-tmenu)
               (:binding "U" :command undo)
               (:binding "," :command causal-editkit-settings-tmenu)
               (:binding "x" :command save-buffers-kill-emacs))))

        (insert "hello")
        (causalt-mock-active-region)
        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-main-tmenu
                                        '(lambda () (random 5000)))
        (save-buffer)))

    (cl-letf (((symbol-function #'buffer-narrowed-p) (lambda () t))
              (causalt-mock #'widen))

      (let ((test-vectors
             '((:binding "W" :command widen))))

        (insert "hello")
        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-main-tmenu
                                        '(lambda () (random 5000)))
        (save-buffer)))
    (causalt-editkit-breakdown tmpfile)))

(provide 'test-causal-editkit)
;;; test-causal-editkit.el ends here
