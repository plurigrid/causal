;;; causal-editkit.el --- Transient user interface library for editing commands -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools, wp

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

;; Causal EditKit is a Transient user interface toolkit for Emacs editing.

;; INSTALLATION
;; (require 'causal-editkit) ; optional if using autoloaded menu
;; (keymap-global-set "C-o" #'causal-editkit-main-tmenu)

;; Alternate bindings to consider are "M-o" and "F10". Choose whatever binding
;; best suits you.

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:
(require 'causal-editkit-utils)

;;;###autoload (autoload 'causal-editkit-main-tmenu "causal-editkit" nil t)
(transient-define-prefix causal-editkit-main-tmenu ()
  "Reference main menu for Causal EditKit.

This prefix is intended to be a reference model that employs the
different Transient prefixes (menus) provided by Causal EditKit.
It can be used as-is or serve as a template for building a
user-customized menu."
  [["File"
    ("o" "Open›" causal-editkit-open-tmenu)
    ("f" "Open file…" find-file-at-point)
    ("d" "Open in Dired" dired-jump-other-window
     :if (lambda () (buffer-file-name)))
    ("b" "List Buffers" ibuffer)
    ("R" "Recent Files" recentf-open-files)
    ("v" "Revert…" revert-buffer
     :inapt-if-not buffer-modified-p
     :if-not (lambda () buffer-read-only))
    ("s" "Save" save-buffer
     :if-not (lambda () buffer-read-only))
    ("S" "Save as…" write-file
     :if-not (lambda () buffer-read-only))
    ("y" "Write region…" write-region
     :if use-region-p)]

   ["Edit"
    :pad-keys t
    ("e" "Edit›" causal-editkit-edit-tmenu)
    ("p" "Fill Paragraph" fill-paragraph
     :if-not causal-editkit-buffer-read-only-p)
    ("l" "Join line" join-line
     :transient nil
     :if-not causal-editkit-buffer-read-only-p)
    ("C-o" "Open line" open-line
     :transient t
     :if-not causal-editkit-buffer-read-only-p)
    ("N" "Narrow›" causal-editkit-narrow-tmenu :if-not buffer-narrowed-p)
    ("W" "Widen" widen :if buffer-narrowed-p)
    ("E" "Emoji & Symbols›" causal-editkit-emoji-symbols-tmenu
     :if-not causal-editkit-buffer-read-only-p)]

   ["Sexp"
    ("m" "Mark" mark-sexp)
    ("c" "Copy" causal-editkit-copy-sexp)
    ("k" "Kill (Cut)" kill-sexp
     :if-not causal-editkit-buffer-read-only-p)
    ("t" "Transpose" transpose-sexps
     :if-not causal-editkit-buffer-read-only-p)]

   ["Tools"
    ("T" "Tools›" causal-editkit-tools-tmenu)
    ("a" "Org Agenda" org-agenda)
    ("C" "Compile…" compile)
    ("*" "Quick Calc…" quick-calc)
    ("!" "Shell Command…" shell-command)
    ("g" "Magit Status" causal-editkit-select-magit-command
     :description causal-editkit-select-magit-command-description
     :if (lambda ()
           (and (causal-editkit-package-magit-installed-p)
                (causal-editkit-version-controlled-p))))
    ("h" "Highlight Symbol" causal-editkit-symbol-overlay-put
     :if causal-editkit-package-symbol-overlay-installed-p)]]

  [[;;"Bookmarks"
    ("B" "Bookmarks›" causal-editkit-bookmarks-tmenu)
    ("J" "Jump to Bookmark…" bookmark-jump)]

   [;;"Window"
    ("w" "Window›" causal-editkit-windows-tmenu)
    ("M-n" "New Frame" make-frame-command)]

   [;;"Search/Replace"
    ("/" "Search/Replace›" causal-editkit-search-tmenu)
    ("P" "Project›" causal-editkit-project-tmenu)]

   [("M" "Macros›" causal-editkit-macro-tmenu)
    ("F" "Font Scale…" text-scale-adjust
     :if display-graphic-p)]]

  ;; causal-editkit-cursor-navigation-group

  [:class transient-row
   (causal-lib-quit-one)
   ("r" "Registers›" causal-editkit-registers-tmenu)
   ("U" "Undo" undo :transient t)
   ("," "Settings›" causal-editkit-settings-tmenu)
   ("RET" "Done" transient-quit-all)
   (causal-lib-quit-all)
   ("x" "Exit Emacs" save-buffers-kill-emacs)])

(provide 'causal-editkit)
;;; causal-editkit.el ends here
