;;; causal-ibuffer.el --- Transient UI for IBuffer -*- lexical-binding: t; -*-

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

;; Causal IBuffer is an opinionated Transient-based porcelain for Emacs IBuffer.

;; INSTALLATION
;; (require 'causal-ibuffer) ; optional if using autoloaded menu
;; (keymap-set ibuffer-mode-map "C-o" #'causal-ibuffer-tmenu)
;; (keymap-set ibuffer-mode-map "F" #'causal-ibuffer-filter-tmenu)
;; (keymap-set ibuffer-mode-map "s" #'causal-ibuffer-sortby-tmenu)

;; While optional, this configuration can be used to align the bindings in
;; `ibuffer-mode-map' with the bindings used by the Causal menus.
;; (keymap-set ibuffer-mode-map "<double-mouse-1>" #'ibuffer-visit-buffer)
;; (keymap-set ibuffer-mode-map "M-<double-mouse-1>" #'ibuffer-visit-buffer-other-window)
;; (keymap-set ibuffer-mode-map "{" #'ibuffer-backwards-next-marked)
;; (keymap-set ibuffer-mode-map "}" #'ibuffer-forward-next-marked)
;; (keymap-set ibuffer-mode-map "[" #'ibuffer-backward-filter-group)
;; (keymap-set ibuffer-mode-map "]" #'ibuffer-forward-filter-group)
;; (keymap-set ibuffer-mode-map "$" #'ibuffer-toggle-filter-group)

;; These are some convenience hooks.
;; (add-hook 'ibuffer-mode-hook #'hl-line-mode)
;; (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:
(require 'ibuffer)
(require 'bookmark)
(require 'causal-lib)
(require 'causal-ibuffer-utils)
(require 'causal-ibuffer-settings)
(require 'causal-ibuffer-filter)

;;;###autoload (autoload 'causal-ibuffer-tmenu "causal-ibuffer" nil t)
(transient-define-prefix causal-ibuffer-tmenu ()
  :refresh-suffixes t
  ["IBuffer: Main"
   ["Operations"
    ("o" "Visit Other" ibuffer-visit-buffer-other-window)
    ("S" "Save" ibuffer-do-save :transient t)
    ("D" "Delete…" ibuffer-do-delete :transient t)

    ("=" "Diff Buffer w/File" ibuffer-diff-with-file)
    ("w" "Copy File Name" ibuffer-copy-filename-as-kill)
    ("M" "More›" causal-ibuffer-operations-tmenu)]

   ["Mark"
    ("m" "Mark" ibuffer-mark-forward :transient t)
    ("t" "Type›" causal-ibuffer-mark-tmenu :transient t)
    ("r" "Regexp›" causal-ibuffer-mark-regexp-tmenu :transient t)
    ("u" "Unmark" ibuffer-unmark-forward :transient t)
    ("d" "For Deletion" ibuffer-mark-for-delete :transient t)
    ("x" "Bulk Delete…" ibuffer-do-kill-on-deletion-marks)
    ("U" "Unmark All" ibuffer-unmark-all-marks :transient t)]

   ["Display"
    ("s" "Sort By›" causal-ibuffer-sortby-tmenu :transient t)
    ("`" "Toggle Format" ibuffer-switch-format :transient t)
    ("b" "Bury Buffer" ibuffer-bury-buffer :transient t)
    ("g" "Refresh" ibuffer-update :transient t)
    ("$" "Toggle Group" ibuffer-toggle-filter-group
     :description (lambda () (format "Toggle %s" (causal-ibuffer-unicode-get :group)))
     :inapt-if-not (lambda () (causal-ibuffer-filter-group-p))
     :transient t)]

   ["Navigation"
    :pad-keys t
    ("p" "Previous" ibuffer-backward-line
     :description (lambda ()
                    (format "%s" (causal-ibuffer-unicode-get :previous)))
     :transient t)
    ("n" "Next" ibuffer-forward-line
     :description (lambda () (format "%s" (causal-ibuffer-unicode-get :next)))
     :transient t)
    ("{" "Previous Marked" ibuffer-backwards-next-marked
     :description (lambda ()
                    (format "%s %s"
                            (causal-ibuffer-unicode-get :previous)
                            (causal-ibuffer-unicode-get :marked)))
     :transient t)
    ("}" "Next Marked" ibuffer-forward-next-marked
     :description (lambda ()
                    (format "%s %s"
                            (causal-ibuffer-unicode-get :next)
                            (causal-ibuffer-unicode-get :marked)))
     :transient t)
    ("[" "Previous Group" ibuffer-backward-filter-group
     :description (lambda ()
                    (format "%s %s"
                            (causal-ibuffer-unicode-get :previous)
                            (causal-ibuffer-unicode-get :group)))
     :transient t)
    ("]" "Next Group" ibuffer-forward-filter-group
     :description (lambda ()
                    (format "%s %s"
                            (causal-ibuffer-unicode-get :next)
                            (causal-ibuffer-unicode-get :group)))
     :transient t)
    ("j" "Jump…" ibuffer-jump-to-buffer
     :description (lambda () (format "%s…" (causal-ibuffer-unicode-get :jump)))
     :transient t)
    ("M-j" "Jump…" ibuffer-jump-to-filter-group
     :description (lambda () (format "%s…"
                                     (causal-ibuffer-unicode-get :jumpgroup)))
     :transient t)]]

  [["Filter"
    :pad-keys t
    ("SPC" "Add Rule…" ibuffer-filter-chosen-by-completion)
    ("/" "Clear" ibuffer-filter-disable)
    ("F" "Filter›" causal-ibuffer-filter-tmenu :transient t)]

   ["Find/Replace in Marked"
    :pad-keys t
    ("O" "Occur…" ibuffer-do-occur)
    ;;("C-s" "I-Search…" ibuffer-do-isearch)
    ;;("C-M-s" "I-Search regexp…" ibuffer-do-isearch-regexp)
    ("M-r" "Query Replace…" ibuffer-do-query-replace)
    ("C-M-r" "Query Replace Regexp…" ibuffer-do-query-replace-regexp)]

   ["Quick"
    ("J" "Jump to Bookmark…" bookmark-jump :transient nil)]]

  [:class transient-row
          (causal-lib-quit-one)
          ("RET" "Visit/Toggle" causal-ibuffer-return-dwim)
          ("," "Settings›" causal-ibuffer-settings-tmenu)
          (causal-lib-quit-all)
          ("q" "Quit IBuffer" quit-window)])


(transient-define-prefix causal-ibuffer-operations-tmenu ()
  ["IBuffer: Operations"
   [("R" "Rename Uniquely…" ibuffer-do-rename-uniquely)
    ("!" "Shell…" ibuffer-do-shell-command-file)
    ("|" "Pipe to Shell…" ibuffer-do-shell-command-pipe)]

   [("E" "Eval" ibuffer-do-eval)
    ("B" "Copy Buffer Name" ibuffer-copy-buffername-as-kill)]

   [("T" "Toggle Read-only" ibuffer-do-toggle-read-only)
    ("L" "Toggle Lock" ibuffer-do-toggle-lock)]]

  [:class transient-row
          (causal-lib-quit-one)
          (causal-lib-quit-all)])

;;;###autoload (autoload 'causal-ibuffer-sortby-tmenu "causal-ibuffer" nil t)
(transient-define-prefix causal-ibuffer-sortby-tmenu ()
  ["IBuffer: Sort By"
   [("v" "Recency" ibuffer-do-sort-by-recency)
    ("a" "Buffer Name" ibuffer-do-sort-by-alphabetic)
    ("f" "Filename/Process" ibuffer-do-sort-by-filename/process)]

   [("m" "Major Mode" ibuffer-do-sort-by-major-mode)
    ("s" "Size" ibuffer-do-sort-by-size)]

   [("i" "Invert" ibuffer-invert-sorting)]]

  [:class transient-row
          (causal-lib-quit-one)
          ("," "Cycle Sort" ibuffer-toggle-sorting-mode)
          (causal-lib-quit-all)])

(transient-define-prefix causal-ibuffer-mark-tmenu ()
  ["Mark By"
   [("m" "Mode" ibuffer-mark-by-mode)
    ("d" "Dired" ibuffer-mark-dired-buffers)
    ("h" "Help" ibuffer-mark-help-buffers)]

   [("*" "Modified" ibuffer-mark-modified-buffers)
    ("r" "Read-only" ibuffer-mark-read-only-buffers)
    ("u" "Unsaved" ibuffer-mark-unsaved-buffers)]

   [("D" "Dissociated" ibuffer-mark-dissociated-buffers)
    ("s" "*Special*" ibuffer-mark-special-buffers)
    ("z" "Compressed" ibuffer-mark-compressed-file-buffers)]]

  [:class transient-row
          (causal-lib-quit-one)
          ("U" "Unmark All" ibuffer-unmark-all-marks :transient t)
          (causal-lib-quit-all)])

(transient-define-prefix causal-ibuffer-mark-regexp-tmenu ()
  ["IBuffer: Mark Regexp"
   ("f" "File name" ibuffer-mark-by-file-name-regexp)
   ("n" "Buffer Name" ibuffer-mark-by-name-regexp)
   ("m" "Mode" ibuffer-mark-by-mode-regexp)
   ("c" "Content" ibuffer-mark-by-content-regexp)]
  [:class transient-row
          (causal-lib-quit-one)
          ("U" "Unmark All" ibuffer-unmark-all-marks :transient t)
          (causal-lib-quit-all)])

(provide 'causal-ibuffer)
;;; causal-ibuffer.el ends here
