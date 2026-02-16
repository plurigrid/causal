;;; causal-ibuffer-utils.el --- Causal IBuffer Utils -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'ibuffer)
(require 'causal-lib)

(defconst causal-ibuffer-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:jump . '("🚀" "Jump"))
    (:marked . '("❯" "Marked"))
    (:group . '("[]" "Group"))
    (:jumpgroup . '("🚀[]" "Jump to Group")))

  "Unicode symbol DB to use for IBuffer Transient menus.")

(defun causal-ibuffer-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `causal-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (causal-lib-unicode-db-get key causal-ibuffer-unicode-db))

(defun causal-ibuffer-return-dwim ()
  "DWIM function when <return> key is pressed for Causal IBuffer.

Inspects if the point is on a filter group and if so will call
`ibuffer-toggle-filter-group', otherwise call
`ibuffer-visit-buffer'."
  (interactive)
  (if (causal-ibuffer-filter-group-p)
      (ibuffer-toggle-filter-group)
    (ibuffer-visit-buffer)))

(defun causal-ibuffer-filter-group-p ()
  "Predicate if filter group is under point."
  (let ((name (get-text-property (point) 'ibuffer-filter-group-name)))
    (if (stringp name) t nil)))


(provide 'causal-ibuffer-utils)
;;; causal-ibuffer-utils.el ends here
