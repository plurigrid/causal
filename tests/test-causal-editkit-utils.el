;;; test-causal-editkit-utils.el --- Causal IBuffer Utils Tests  -*- lexical-binding: t; -*-

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
(require 'causal-editkit-utils)

(ert-deftest test-causal-editkit-open-tmenu ()
  (let ((tmpfile "causal-editkit-open-tmenu.txt"))
    (causalt-editkit-setup tmpfile)

    (cl-letf (;; ((symbol-function #'buffer-file-name) (lambda () t))
              (causalt-mock #'find-file)
              (causalt-mock #'rename-visited-file)
              (causalt-mock #'find-file-other-window)
              (causalt-mock #'find-file-other-frame)
              (causalt-mock #'find-alternate-file)
              (causalt-mock #'insert-file)
              (causalt-mock #'kill-buffer))

      (let ((test-vectors
             '((:binding "f" :command find-file)
               (:binding "r" :command rename-visited-file)
               (:binding "R" :command rename-buffer)
               (:binding "M-r" :command rename-uniquely)
               (:binding "F" :command find-file-other-window)
               (:binding "M-n" :command find-file-other-frame)
               (:binding "a" :command find-alternate-file)
               (:binding "i" :command insert-file)
               (:binding "c" :command kill-buffer))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-open-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))


(ert-deftest test-causal-editkit-project-tmenu ()
  (let ((tmpfile "causal-editkit-project-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf (;;((symbol-function #') (lambda () t))
              (causalt-mock #'project-find-file)
              (causalt-mock #'project-dired)
              (causalt-mock #'project-find-dir)
              (causalt-mock #'project-vc-dir)
              (causalt-mock #'project-find-regexp)
              (causalt-mock #'project-query-replace-regexp)
              (causalt-mock #'project-search)
              (causalt-mock #'fileloop-continue)
              (causalt-mock #'project-compile)
              (causalt-mock #'project-shell-command)
              (causalt-mock #'project-async-shell-command)
              (causalt-mock #'project-eshell)
              (causalt-mock #'project-shell)
              (causalt-mock #'project-switch-project)
              (causalt-mock #'project-list-buffers)
              (causalt-mock #'project-kill-buffers)
              (causalt-mock #'project-forget-project)
              (causalt-mock #'project-switch-to-buffer))

      (let ((test-vectors
             '((:binding "f" :command project-find-file)
               (:binding "Bcausal" :command project-switch-to-buffer)
               (:binding "d" :command project-dired)
               (:binding "D" :command project-find-dir)
               (:binding "v" :command project-vc-dir)
               (:binding "r" :command project-find-regexp)
               (:binding "q" :command project-query-replace-regexp)
               (:binding "S" :command project-search)
               (:binding "n" :command fileloop-continue)
               (:binding "c" :command project-compile)
               (:binding "!" :command project-shell-command)
               (:binding "&" :command project-async-shell-command)
               (:binding "e" :command project-eshell)
               (:binding "M-s" :command project-shell)
               ;; (:binding "s" :command project-switch-project)
               (:binding "b" :command project-list-buffers)
               (:binding "k" :command project-kill-buffers)
               (:binding "FC-g" :command project-forget-project))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-project-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-edit-tmenu ()
  (let ((tmpfile "causal-editkit-edit-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ((causalt-mock #'duplicate-dwim)
              (causalt-mock #'flush-lines)
              (causalt-mock #'keep-lines)
              (causalt-mock #'fill-paragraph)
              (causalt-mock #'yank)
              (causalt-mock #'align-regexp))

      (let ((test-vectors
             '((:binding "m" :command causal-editkit-mark-tmenu)
               (:binding "c" :command causal-editkit-copy-tmenu)
               (:binding "k" :command causal-editkit-kill-tmenu)
               (:binding "y" :command yank)
               (:binding "t" :command causal-editkit-transpose-tmenu)
               (:binding "T" :command causal-editkit-transform-text-tmenu)
               (:binding "v" :command causal-editkit-move-text-tmenu)
               (:binding "d" :command causal-editkit-delete-tmenu)
               (:binding "s" :command causal-editkit-sort-tmenu)
               (:binding "D" :command duplicate-dwim)
               (:binding "F" :command flush-lines)
               (:binding "K" :command keep-lines)
               (:binding "f" :command fill-paragraph)
               (:binding "a" :command align-regexp)
               (:binding "R" :command causal-editkit-rectangle-tmenu)
               (:binding "r" :command causal-editkit-reformat-tmenu))))
        (causalt-mock-active-region)
        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-edit-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-emoji-symbols-tmenu ()
  (let ((tmpfile "causal-editkit-emoji-symbols-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (emacs-lisp-mode)
    (cl-letf ((causalt-mock #'emoji-search)
              (causalt-mock #'electric-quote-mode))
      (let ((test-vectors
             '((:binding "e😀" :command emoji-search)
               (:binding "l" :command emoji-list)
               (:binding "d" :command emoji-describe)
               (:binding "+" :command emoji-zoom-increase)
               (:binding "-" :command emoji-zoom-decrease)
               (:binding "0" :command emoji-zoom-reset)
               (:binding "'" :command causal-editkit-smart-single-quote-dwim)
               (:binding "\"" :command causal-editkit-smart-double-quote-dwim)
               (:binding "_" :command causal-editkit-smart-low-quote-dwim)
               (:binding "c" :command causal-editkit-smart-comillas-quote-dwim)
               (:binding "Q" :command electric-quote-mode)
               ;;(:binding "i" :command insert-char)
               )))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-emoji-symbols-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-mark-tmenu ()
  (let ((tmpfile "causal-editkit-mark-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (emacs-lisp-mode)
    (cl-letf ((causalt-mock #'mark-word)
              (causalt-mock #'mark-end-of-sentence)
              (causalt-mock #'mark-paragraph)
              (causalt-mock #'mark-sexp)
              (causalt-mock #'mark-defun))

      (let ((test-vectors
             '((:binding "w" :command mark-word)
               (:binding "s" :command mark-end-of-sentence)
               (:binding "p" :command mark-paragraph)
               (:binding "b" :command mark-sexp)
               (:binding "d" :command mark-defun))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-mark-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-sort-tmenu ()
  (let ((tmpfile "causal-editkit-sort-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ((causalt-mock #'sort-lines)
              (causalt-mock #'sort-paragraphs)
              (causalt-mock #'sort-pages)
              (causalt-mock #'sort-fields)
              (causalt-mock #'sort-numeric-fields)
              (causalt-mock #'sort-regexp-fields)
              (causalt-mock #'sort-columns)
              (causalt-mock #'reverse-region))

      (let ((test-vectors
             '((:binding "l" :command sort-lines)
               (:binding "p" :command sort-paragraphs)
               (:binding "P" :command sort-pages)
               (:binding "f" :command sort-fields)
               (:binding "n" :command sort-numeric-fields)
               (:binding "r" :command sort-regexp-fields)
               (:binding "c" :command sort-columns)
               (:binding "-" :command reverse-region))))

        (causalt-mock-active-region)
        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-sort-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))


(ert-deftest test-causal-editkit-copy-tmenu ()
  (let ((tmpfile "causal-editkit-copy-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (emacs-lisp-mode)
    (cl-letf ((causalt-mock #'kill-ring-save)
              (causalt-mock #'copy-matching-lines))
      (let ((test-vectors
             '((:binding "w" :command causal-editkit-copy-word)
               (:binding "s" :command causal-editkit-copy-sentence)
               (:binding "p" :command causal-editkit-copy-paragraph)
               (:binding "b" :command causal-editkit-copy-sexp)
               (:binding "d" :command causal-editkit-copy-defun)
               (:binding "m" :command copy-matching-lines)
               (:binding "r" :command kill-ring-save))))

        (causalt-mock-active-region)
        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-copy-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-kill-tmenu ()
  (let ((tmpfile "causal-editkit-kill-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ((causalt-mock #'kill-word)
              (causalt-mock #'kill-sentence)
              (causalt-mock #'kill-paragraph)
              (causalt-mock #'kill-line)
              (causalt-mock #'kill-matching-lines)
              (causalt-mock #'kill-sexp)
              (causalt-mock #'kill-region))

      (let ((test-vectors
             '((:binding "w" :command kill-word)
               (:binding "s" :command kill-sentence)
               (:binding "p" :command kill-paragraph)
               (:binding "l" :command kill-line)
               (:binding "b" :command kill-sexp)
               (:binding "m" :command kill-matching-lines)
               (:binding "r" :command kill-region))))

        (causalt-mock-active-region)
        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-kill-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-transpose-tmenu ()
  (let ((tmpfile "causal-editkit-transpose-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ((causalt-mock #'transpose-chars)
              (causalt-mock #'transpose-words)
              (causalt-mock #'transpose-lines)
              (causalt-mock #'transpose-sentences)
              (causalt-mock #'transpose-paragraphs)
              (causalt-mock #'transpose-sexps)
              (causalt-mock #'transpose-regions))

      (let ((test-vectors
             '((:binding "c" :command transpose-chars)
               (:binding "w" :command transpose-words)
               (:binding "l" :command transpose-lines)
               (:binding "s" :command transpose-sentences)
               (:binding "p" :command transpose-paragraphs)
               (:binding "b" :command transpose-sexps)
               ;;(:binding "r" :command transpose-regions) ; TODO mock
               )))
        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-transpose-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-delete-tmenu ()
  (let ((tmpfile "causal-editkit-delete-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ((causalt-mock #'just-one-space)
              (causalt-mock #'join-line)
              (causalt-mock #'delete-horizontal-space)
              (causalt-mock #'delete-pair)
              (causalt-mock #'delete-blank-lines)
              (causalt-mock #'whitespace-cleanup)
              (causalt-mock #'delete-trailing-whitespace)
              (causalt-mock #'zap-up-to-char)
              (causalt-mock #'zap-to-char))

      (let ((test-vectors
             '((:binding "o" :command just-one-space)
               (:binding "j" :command join-line)
               (:binding "h" :command delete-horizontal-space)
               (:binding "p" :command delete-pair)
               (:binding "b" :command delete-blank-lines)
               (:binding "w" :command whitespace-cleanup)
               (:binding "d" :command delete-trailing-whitespace)
               (:binding "z" :command zap-up-to-char)
               (:binding "Z" :command zap-to-char))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-delete-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-move-text-tmenu ()
  (let ((tmpfile "causal-editkit-move-text-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ()
      (let ((test-vectors
             '((:binding "w" :command causal-editkit-move-word-tmenu)
               (:binding "s" :command causal-editkit-move-sentence-tmenu)
               (:binding "b" :command causal-editkit-move-sexp-tmenu))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-move-text-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-move-word-tmenu ()
  (let ((tmpfile "causal-editkit-move-word-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ()

      (let ((test-vectors
             '((:binding "b" :command causal-editkit-move-word-backward)
               (:binding "f" :command causal-editkit-move-word-forward)
               (:binding "RET" :command transient-quit-all))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-move-word-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-move-sentence-tmenu ()
  (let ((tmpfile "causal-editkit-move-sentence-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ()

      (let ((test-vectors
             '((:binding "b" :command causal-editkit-move-sentence-backward)
               (:binding "f" :command causal-editkit-move-sentence-forward)
               (:binding "RET" :command transient-quit-all))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-move-sentence-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-move-sexp-tmenu ()
  (let ((tmpfile "causal-editkit-move-sexp-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ()

      (let ((test-vectors
             '((:binding "b" :command causal-editkit-move-sexp-backward)
               (:binding "f" :command causal-editkit-move-sexp-forward)
               (:binding "RET" :command transient-quit-all))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-move-sexp-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))


(ert-deftest test-causal-editkit-windows-tmenu ()
  (let ()
    (split-window-horizontally)
    (split-window-vertically)
    (cl-letf ((causalt-mock #'other-window)
              (causalt-mock #'causal-editkit-windows-delete-tmenu)
              (causalt-mock #'windmove-down)
              (causalt-mock #'windmove-up)
              (causalt-mock #'windmove-right)
              (causalt-mock #'windmove-left)
              (causalt-mock #'window-swap-states)
              (causalt-mock #'window-swap-states-left)
              (causalt-mock #'window-swap-states-right)
              (causalt-mock #'window-swap-states-up)
              (causalt-mock #'window-swap-states-down)

              (causalt-mock #'split-window-below)
              (causalt-mock #'split-window-horizontally)
              (causalt-mock #'causal-editkit-windows-delete-tmenu)
              (causalt-mock #'enlarge-window)
              (causalt-mock #'shrink-window)
              (causalt-mock #'enlarge-window-horizontally)
              (causalt-mock #'shrink-window-horizontally)
              (causalt-mock #'delete-other-windows))

      (let ((test-vectors
             '((:binding "o" :command other-window)
               (:binding "n" :command windmove-down)
               (:binding "p" :command windmove-up)
               (:binding "f" :command windmove-right)
               (:binding "b" :command windmove-left)
               (:binding "s" :command window-swap-states)
               (:binding "o" :command other-window)
               ;;(:binding "M-b" :command window-swap-states-left)
               ;;(:binding "M-f" :command window-swap-states-right)
               ;;(:binding "M-p" :command window-swap-states-up)
               ;;(:binding "M-n" :command window-swap-states-down)

               (:binding "2" :command split-window-below)
               (:binding "3" :command split-window-horizontally)
               (:binding "DEL" :command causal-editkit-windows-delete-tmenu)

               (:binding "+" :command enlarge-window)
               (:binding "-" :command shrink-window)

               (:binding "[" :command enlarge-window-horizontally)
               (:binding "]" :command shrink-window-horizontally)

               (:binding "1" :command delete-other-windows)
               (:binding "RET" :command transient-quit-all))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-windows-tmenu
                                        '(lambda () (random 5000)))))

    (delete-other-windows)
    ;;(causalt-editkit-breakdown)
    ))

;; (ert-deftest test-causal-editkit-windows-tmenu ()
;;   (let ((tmpfile "causal-editkit-windows-tmenu.txt"))
;;     (causalt-editkit-setup tmpfile)
;;     (cl-letf ((causalt-mock #'other-window)
;;               (causalt-mock #'windmove-up)
;;               (causalt-mock #'windmove-down)
;;               (causalt-mock #'windmove-left)
;;               (causalt-mock #'windmove-right)

;;               (causalt-mock #'window-swap-states)
;;               (causalt-mock #'windmove-swap-states-up)
;;               (causalt-mock #'windmove-swap-states-down)
;;               (causalt-mock #'windmove-swap-states-left)
;;               (causalt-mock #'windmove-swap-states-right)

;;               (causalt-mock #'delete-other-windows)
;;               (causalt-mock #'split-window-below)
;;               (causalt-mock #'split-window-horizontally)

;;               (causalt-mock #'transpose-frame)

;;               (causalt-mock #'enlarge-window)
;;               (causalt-mock #'shrink-window)
;;               (causalt-mock #'enlarge-window-horizontally)
;;               (causalt-mock #'shrink-window-horizontally))

;;       (let ((test-vectors
;;              '((:binding "o" :command other-window)
;;                (:binding "p" :command windmove-up)
;;                (:binding "n" :command windmove-down)
;;                (:binding "b" :command windmove-left)
;;                (:binding "f" :command windmove-right)

;;                (:binding "s" :command window-swap-states)
;;                (:binding "M-p" :command windmove-swap-states-up)
;;                (:binding "M-n" :command windmove-swap-states-down)
;;                (:binding "M-b" :command windmove-swap-states-left)
;;                (:binding "M-f" :command windmove-swap-states-right)

;;                (:binding "1" :command delete-other-windows)
;;                (:binding "2" :command split-window-below)
;;                (:binding "3" :command split-window-horizontally)

;;                (:binding "t" :command transpose-frame)
;;                (:binding "d" :command causal-editkit-windows-delete-tmenu)

;;                (:binding "+" :command enlarge-window)
;;                (:binding "-" :command shrink-window)
;;                (:binding ">" :command enlarge-window-horizontally)
;;                (:binding "<" :command shrink-window-horizontally))))

;;         (split-window-below)
;;         (causalt-suffix-testcase-runner test-vectors
;;                                         #'causal-editkit-windows-tmenu
;;                                         '(lambda () (random 5000)))))
;;     (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-windows-delete-tmenu ()
  (let ((tmpfile "causal-editkit-windows-delete-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ((causalt-mock #'windmove-delete-up)
              (causalt-mock #'windmove-delete-down)
              (causalt-mock #'windmove-delete-left)
              (causalt-mock #'windmove-delete-right))

      (let ((test-vectors
             '((:binding "p" :command windmove-delete-up)
               (:binding "n" :command windmove-delete-down)
               (:binding "b" :command windmove-delete-left)
               (:binding "f" :command windmove-delete-right))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-windows-delete-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-bookmarks-tmenu ()
  (let ((tmpfile "causal-editkit-bookmarks-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ((causalt-mock #'bookmark-set-no-overwrite)
              (causalt-mock #'bookmark-jump))

      (let ((test-vectors
             '((:binding "e" :command causal-editkit-list-bookmarks-transient)
               (:binding "a" :command bookmark-set-no-overwrite)
               (:binding "J" :command bookmark-jump))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-bookmarks-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-search-tmenu ()
  (let ((tmpfile "causal-editkit-search-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ((causalt-mock #'occur)
              (causalt-mock #'find-name-dired)
              (causalt-mock #'find-grep-dired)
              (causalt-mock #'rgrep))

      (let ((test-vectors
             '((:binding "s" :command causal-editkit--isearch)
               (:binding "S" :command causal-editkit--search)
               (:binding "r" :command causal-editkit--query-replace)
               (:binding "o" :command occur)
               (:binding "d" :command find-name-dired)
               (:binding "G" :command find-grep-dired)
               (:binding "g" :command rgrep))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-search-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

(ert-deftest test-causal-editkit-tools-tmenu ()
  (let ((tmpfile "causal-editkit-tools-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf (;;((symbol-function #') (lambda () t))
              (causalt-mock #'shell)
              (causalt-mock #'shell-command)
              (causalt-mock #'async-shell-command)
              (causalt-mock #'eshell)
              (causalt-mock #'ielm)
              (causalt-mock #'term)
              (causalt-mock #'run-python)
              (causalt-mock #'calc)
              (causalt-mock #'weather)
              (causalt-mock #'re-builder)
              (causalt-mock #'count-words)
              (causalt-mock #'calendar)
              (causalt-mock #'world-clock)
              (causalt-mock #'sunrise-sunset)
              (causalt-mock #'erc)
              (causalt-mock #'eww)
              (causalt-mock #'causal-timezone-tmenu)
              (causalt-mock #'tetris)
              (causalt-mock #'zone))

      (let ((test-vectors
             '((:binding "sh" :command shell)
               (:binding "!" :command shell-command)
               (:binding "&" :command async-shell-command)
               (:binding "es" :command eshell)
               (:binding "ie" :command ielm)
               (:binding "te" :command term)
               (:binding "py" :command run-python)
               (:binding "cc" :command calc)
               (:binding "re" :command re-builder)
               (:binding "wc" :command count-words)
               (:binding "ca" :command calendar)
               (:binding "cl" :command world-clock)
               (:binding "su" :command sunrise-sunset)
               (:binding "ery" :command erc)
               (:binding "ew" :command eww)
               (:binding "tz" :command causal-timezone-tmenu)
               (:binding "zo" :command zone)
               (:binding "ts" :command tetris))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-tools-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))

;; (ert-deftest test-causal-editkit-registers-tmenu ()
;;   (let ((tmpfile "causal-editkit-registers-tmenu.txt"))
;;     (causalt-editkit-setup tmpfile)
;;     (cl-letf ((causalt-mock #'point-to-register)
;;               (causalt-mock #'window-configuration-to-register)
;;               (causalt-mock #'kmacro-to-register)
;;               (causalt-mock #'jump-to-register)
;;               (causalt-mock #'copy-to-register)
;;               (causalt-mock #'copy-rectangle-to-register)
;;               (causalt-mock #'append-to-register)
;;               (causalt-mock #'prepend-to-register)
;;               (causalt-mock #'insert-register))

;;       (let ((test-vectors
;;              '(
;;                (:binding "p" :command point-to-register)
;;                (:binding "w" :command window-configuration-to-register)
;;                (:binding "m" :command kmacro-to-register)
;;                (:binding "j" :command jump-to-register)
;;                (:binding "c" :command copy-to-register)
;;                (:binding "r" :command copy-rectangle-to-register)
;;                (:binding "a" :command append-to-register)
;;                (:binding "P" :command prepend-to-register)
;;                (:binding "i" :command insert-register))))

;;         (causalt-mock-active-region)
;;         (causalt-suffix-testcase-runner test-vectors
;;                                         #'causal-editkit-registers-tmenu
;;                                         '(lambda () (random 5000)))))
;;     (causalt-editkit-breakdown tmpfile)))


(ert-deftest test-causal-editkit-rectangle-tmenu ()
  (let ((tmpfile "causal-editkit-rectangle-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf (((symbol-function #'use-region-p) (lambda () t))
              (causalt-mock #'kill-rectangle)
              (causalt-mock #'copy-rectangle-as-kill)
              (causalt-mock #'yank-rectangle)
              (causalt-mock #'kill-rectangle)
              (causalt-mock #'copy-rectangle-as-kill)
              (causalt-mock #'yank-rectangle)
              (causalt-mock #'delete-rectangle)
              (causalt-mock #'string-rectangle)
              (causalt-mock #'string-insert-rectangle)
              (causalt-mock #'open-rectangle)
              (causalt-mock #'rectangle-mark-mode)
              (causalt-mock #'rectangle-number-lines)
              (causalt-mock #'clear-rectangle)
              (causalt-mock #'delete-whitespace-rectangle))

      (let ((test-vectors
             '((:binding "k" :command kill-rectangle)
               (:binding "c" :command copy-rectangle-as-kill)
               (:binding "y" :command yank-rectangle)
               (:binding "d" :command delete-rectangle)
               (:binding "s" :command string-rectangle)
               (:binding "i" :command string-insert-rectangle)
               (:binding "o" :command open-rectangle)
               (:binding "m" :command rectangle-mark-mode)
               (:binding "N" :command rectangle-number-lines)
               (:binding "C" :command clear-rectangle)
               (:binding "D" :command delete-whitespace-rectangle)
               (:binding "RET" :command transient-quit-all))))

        (set-mark-command nil)
        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-rectangle-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))


(ert-deftest test-causal-editkit-transform-text-tmenu ()
  (let ((tmpfile "causal-editkit-transform-text-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ((causalt-mock #'capitalize-dwim)
              (causalt-mock #'upcase-initials-region)
              (causalt-mock #'downcase-dwim)
              (causalt-mock #'upcase-dwim))

      (let ((test-vectors
             '((:binding "c" :command capitalize-dwim)
               (:binding "t" :command upcase-initials-region)
               (:binding "l" :command downcase-dwim)
               (:binding "u" :command upcase-dwim)
               (:binding "RET" :command transient-quit-all))))

        (causalt-mock-active-region)
        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-transform-text-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))


(ert-deftest test-causal-editkit-reformat-tmenu ()
  (let ((tmpfile "causal-editkit-reformat-tmenu.txt"))
    (causalt-editkit-setup tmpfile)
    (cl-letf (((symbol-function #'use-region-p) (lambda () t))
              (causalt-mock #'fill-paragraph)
              (causalt-mock #'fill-region)
              (causalt-mock #'fill-region-as-paragraph)
              (causalt-mock #'fill-individual-paragraphs)
              (causalt-mock #'fill-nonuniform-paragraphs)
              (causalt-mock #'center-line)
              (causalt-mock #'center-region)
              (causalt-mock #'center-paragraph)
              (causalt-mock #'repunctuate-sentences)
              (causalt-mock #'untabify)
              (causalt-mock #'auto-fill-mode)
              (causalt-mock #'set-fill-column))

      (let ((test-vectors
             '((:binding "p" :command fill-paragraph)
               (:binding "r" :command fill-region)
               (:binding "P" :command fill-region-as-paragraph)
               (:binding "M-p" :command fill-individual-paragraphs)
               (:binding "n" :command fill-nonuniform-paragraphs)
               (:binding "C-l" :command center-line)
               (:binding "C-r" :command center-region)
               (:binding "C-p" :command center-paragraph)
               (:binding "R" :command repunctuate-sentences)
               (:binding "u" :command untabify)
               (:binding "a" :command auto-fill-mode)
               (:binding "d" :command causal-editkit--customize-sentence-end-double-space)
               (:binding "C" :command set-fill-column)

               )))
        (set-mark-command nil)
        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-reformat-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-editkit-breakdown tmpfile)))


(ert-deftest test-causal-editkit-narrow-tmenu ()
  ;; TODO: need to test for region.
  (let ((tmpfile "causal-editkit-narrow-tmenu.el"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ((causalt-mock #'narrow-to-defun)
              (causalt-mock #'narrow-to-page))
      (let ((test-vectors
             '((:binding "d" :command narrow-to-defun)
               (:binding "p" :command narrow-to-page))))
        (put 'narrow-to-page 'disabled nil)
        (put 'narrow-to-region 'disabled nil)
        (emacs-lisp-mode)
        (insert "(defun foo() (message \"hi.\"))")
        (goto-char (point-min))
        (save-buffer)

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-narrow-tmenu
                                        '(lambda () (random 5000)))))

    (causalt-editkit-breakdown tmpfile))

  (let ((tmpfile "causal-editkit-narrow-tmenu.org"))
    (causalt-editkit-setup tmpfile)
    (cl-letf ((causalt-mock #'org-narrow-to-subtree)
              (causalt-mock #'org-narrow-to-block)
              (causalt-mock #'org-narrow-to-element))

      (let ((test-vectors
             '((:binding "s" :command org-narrow-to-subtree)
               ;; (:binding "e" :command org-narrow-to-element)
               )))

        (org-mode)
        (insert "* Header 1\n")

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-narrow-tmenu
                                        '(lambda () (random 5000))))
      (widen)
      (let ((test-vectors
             '((:binding "b" :command org-narrow-to-block)
               ;; (:binding "e" :command org-narrow-to-element)
               )))

        (insert "#+begin_src elisp\n")
        (insert "hi there\n")
        (insert "#+end_src\n")
        (previous-line 2)

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-narrow-tmenu
                                        '(lambda () (random 5000))))
      (widen)
      (let ((test-vectors
             '((:binding "e" :command org-narrow-to-element))))

        (insert "This is a bogus sentence.")
        (beginning-of-line)

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-editkit-narrow-tmenu
                                        '(lambda () (random 5000))))
      (widen)
      (save-buffer)
      (causalt-editkit-breakdown tmpfile)))


  ;; (let ((tmpfile "causal-editkit-narrow-tmenu.md"))
  ;;   (causalt-editkit-setup tmpfile)
  ;;   (cl-letf ((causalt-mock #'markdown-narrow-to-subtree)
  ;;             (causalt-mock #'markdown-narrow-to-block)
  ;;             (causalt-mock #'markdown-narrow-to-page))

  ;;     (let ((test-vectors
  ;;            '((:binding "s" :command markdown-narrow-to-subtree)

  ;;              )))

  ;;       (markdown-mode)
  ;;       (insert "# Header 1\n")

  ;;       (causalt-suffix-testcase-runner test-vectors
  ;;                                       #'causal-editkit-narrow-tmenu
  ;;                                       '(lambda () (random 5000))))

  ;;     (widen)
  ;;     (let ((test-vectors
  ;;            '((:binding "b" :command markdown-narrow-to-block)
  ;;              ;; (:binding "e" :command org-narrow-to-element)
  ;;              )))

  ;;       (insert "    #!python\n")
  ;;       (insert "    def foo(a):\n")
  ;;       (insert "        return a + 2\n")
  ;;       (previous-line 2)

  ;;       (causalt-suffix-testcase-runner test-vectors
  ;;                                       #'causal-editkit-narrow-tmenu
  ;;                                       '(lambda () (random 5000))))

  ;;     (widen)

  ;;     (let ((test-vectors
  ;;            '((:binding "p" :command markdown-narrow-to-page))))

  ;;       (insert "This is a bogus sentence.")
  ;;       (beginning-of-line)

  ;;       (causalt-suffix-testcase-runner test-vectors
  ;;                                       #'causal-editkit-narrow-tmenu
  ;;                                       '(lambda () (random 5000))))
  ;;     (widen)
  ;;     (save-buffer)
  ;;     (causalt-editkit-breakdown tmpfile)))
  )

;; -------
(provide 'test-causal-editkit-utils)
;;; test-causal-editkit-utils.el ends here
