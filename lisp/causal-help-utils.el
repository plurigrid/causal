;;; causal-help-utils.el --- Causal Help Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charles Y. Choi

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
(require 'help-mode)
(require 'causal-lib)

(defconst causal-help-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:forward . '("→" "Forward"))
    (:backward . '("←" "Backward"))
    (:goto . '("→" "Goto…"))
    (:link . '("🔗" "Link"))
    (:beginning-of-buffer . '("⇱" "Beginning"))
    (:end-of-buffer . '("⇲" "End"))
    (:paragraph . '("¶" "Paragraph"))
    (:page . '("📄" "Page")))

  "Unicode symbol DB to use for Help Transient menus.")

(defun causal-help-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `causal-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (causal-lib-unicode-db-get key causal-help-unicode-db))


(defun causal-help-info ()
  "Open Info for Emacs Help Mode."
  (interactive) (info "(emacs) Help Mode"))

(defun causal-help--current-data-p ()
  "Predicate if help mode current data exists."
  help-mode--current-data)

(defun causal-help--symbolp ()
  "Predicate if current help mode has a symbol."
  (let ((sym (plist-get help-mode--current-data :symbol)))
    (or (boundp sym) (facep sym))))

(provide 'causal-help-utils)
;;; causal-help-utils.el ends here
