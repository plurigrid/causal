;;; causal-re-builder-utils.el --- Causal Re-Builder Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Charles Y. Choi

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
(require 'simple)
(require 'transient)
(require 're-builder)
(require 'causal-lib)

(defconst causal-re-builder-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next")))

  "Unicode symbol DB to use for RE-Builder Transient menus.")

(defun causal-re-builder-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `causal-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (causal-lib-unicode-db-get key causal-re-builder-unicode-db))

(defun causal-re-builder-interactive-export-p ()
  "Predicate to support interactive export of regexp."
  (and (not (derived-mode-p 'reb-lisp-mode))
       (string= reb-re-syntax "string")))

(defun causal-re-builder-copy ()
  "Reformat `reb-copy' result for Emacs regexp interactive use.

The implementation of `reb-copy' presumes that its result will be
used in Elisp code and as such escapes certain characters.

Often it is desired to instead use the regexp in an interactive
function such as `query-replace-regexp'. Such functions require
that the regexp not be escaped, which motivates the need for this
function."
  (interactive)
  (reb-copy)
  (let* ((buf (pop kill-ring))
         (buf (causal-re-builder--re-elisp-to-interactive buf)))
    (message "Copied %s to kill-ring" buf)
    (kill-new buf)))

(defun causal-re-builder-grep-copy ()
  "Reformat `reb-copy' result for GNU grep-style regex interactive use.

This is useful for commands such as `dired-do-find-regexp' and
`dired-do-find-regexp-and-replace' that take a grep-style regex argument."
  (interactive)
  (reb-copy)
  (let* ((buf (pop kill-ring))
         (buf (causal-re-builder--re-elisp-to-grep-interactive buf)))
    (message "Copied %s to kill-ring" buf)
    (kill-new buf)))

(defun causal-re-builder--re-elisp-to-interactive (s)
  "Convert Elisp regexp S to interactive regexp."
  (let* ((buf s)
         (buf (string-trim-left buf "\""))
         (buf (string-trim-right buf "\""))
         (buf (replace-regexp-in-string (rx "\\" (group anything)) "\\1" buf)))
    buf))

(defun causal-re-builder--re-elisp-to-grep-interactive (s)
  "Convert Elisp regexp S to interactive GNU grep-style regex."
  (let* ((buf s)
         (buf (string-trim-left buf "\""))
         (buf (string-trim-right buf "\""))
         (buf (string-replace "\\\\(" "\x1c" buf))
         (buf (string-replace "\\\\)" "\x1d" buf))
         (buf (replace-regexp-in-string (rx "\\" (group anything)) "\\1" buf))
         (buf (string-replace "\\\"" "\"" buf))
         (buf (string-replace "\x1c" "\\(" buf))
         (buf (string-replace "\x1d" "\\)" buf)))
    buf))

(defun causal-re-builder-regexp-info ()
  "Get Info for syntax of regexps."
  (interactive)
  (info "(elisp) Syntax of Regexps"))

(defun causal-re-builder-rx-info ()
  "Get Info for Rx notation."
  (interactive)
  (info "(elisp) Rx Notation"))

(defun causal-reb-occur ()
  "Run `occur' with the current `reb-regexp' in the `re-builder' target buffer.

This code is taken from Argletrough post on Reddit.
URL `https://www.reddit.com/r/emacs/comments/1j2qs3f/integration_of_rebuilder_and_occur_use_rx_syntax/'"
  (interactive)
  (let ((target-buffer reb-target-buffer)
        (regexp (with-current-buffer reb-target-buffer reb-regexp)))
    (reb-quit)
    (switch-to-buffer target-buffer)
    (occur regexp)))

(provide 'causal-re-builder-utils)
;;; causal-re-builder-utils.el ends here
