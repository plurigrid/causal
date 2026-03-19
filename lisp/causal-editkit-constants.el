;;; causal-editkit-constants.el --- Constants file for Causal EditKit  -*- lexical-binding: t; -*-

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

;;

;;; Code:
(require 'simple)
(require 'causal-lib)

(defconst causal-editkit-unicode-db
  '((:previous . '("↑" "previous"))
    (:next . '("↓" "next"))
    (:point-up . '("↑" "Up"))
    (:point-down . '("↓" "Down"))
    (:point-left . '("←" "Left"))
    (:point-right . '("→" "Right"))

    (:window-above . '("↑" "Above"))
    (:window-below . '("↓" "Below"))
    (:window-left . '("←" "To Left"))
    (:window-right . '("→" "To Right"))

    (:other-window . '("»" "Other"))
    (:delete-other-windows . '("❏" "Delete other"))
    (:split-window-below . '("━" "Below"))
    (:split-window-horizontally . '("┃" "Right"))
    (:enlarge . '("+" "Enlarge"))
    (:shrink . '("−" "Shrink"))
    (:horizontal . '("↔︎" "Horizontal"))
    (:vertical . '("↕︎" "Vertical"))
    (:first . '("⤒" "first"))
    (:last . '("⤓" "last"))
    (:swap . '("⇄" "Swap"))
    (:jump . '("🚀" "Jump"))
    (:balance . '("⊞" "Balance"))
    (:undo-layout . '("↶" "Undo"))
    (:redo-layout . '("↷" "Redo"))
    (:zoom . '("⤢" "Zoom"))
    (:save-layout . '("⎘" "Save"))
    (:restore-layout . '("⎗" "Restore"))
    (:golden-ratio . '("φ" "Golden")))
  "Unicode symbol DB to use for Bookmarks Transient menus.")

(defun causal-editkit-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `causal-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (causal-lib-unicode-db-get key causal-editkit-unicode-db))

;; Transient navigation group for Causal EditKit menus.
(transient-define-group causal-editkit-navigation-group
  [:class transient-row
   (causal-lib-quit-one)
   ("U" "Undo" undo :transient t)
   ("RET" "Done" transient-quit-all)
   (causal-lib-quit-all)])

;; Transient cursor navigation group for Causal EditKit menus.
(transient-define-group causal-editkit-cursor-navigation-group
  ["Cursor"
   :class transient-row
   ("<left>" "←" backward-char :transient t)
   ("<right>" "→" forward-char :transient t)
   ("<up>" "↑" previous-line :transient t)
   ("<down>" "↓" next-line :transient t)])

(provide 'causal-editkit-constants)
;;; causal-editkit-constants.el ends here
