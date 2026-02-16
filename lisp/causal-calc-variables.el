;;; causal-calc-variables.el --- Causal Variable Menu     -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Charles Y. Choi

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
(require 'calc)
(require 'causal-calc--calc)
(require 'transient)
(require 'causal-lib)
(require 'causal-calc-labels)
(require 'causal-calc-utils)

(transient-define-prefix causal-calc-variable-crud-tmenu ()
  "Stored variable operations menu.
Operations to store, recall, clear, and edit variables are provided by this
menu."
  ["Variable Operations"
   [("s" "Store (𝟣:)…" causal-calc--store :transient nil)
    ("r" "Recall…" causal-calc--recall :transient nil)
    ("c" "Clear…" causal-calc--unstore :transient nil)]
   [("e" "Edit…" causal-calc--edit-variable :transient nil)
    ("o" "Copy to other variable…" causal-calc--copy-variable :transient t)
    ("x" "Exchange (𝟣:) to variable…" causal-calc--store-exchange :transient t)]

   [("p" "Persist…" causal-calc--permanent-variable :transient t)
    ("O" "Open Calc Settings File" causal-calc-open-settings-file :transient nil)
    ("i" "Insert variables into buffer…" causal-calc--insert-variables :transient nil)]]

  [:class transient-row
   (causal-lib-quit-one)
   (causal-calc-algebraic-entry)
   (causal-calc-pop)
   (causal-calc-undo-suffix)
   (causal-lib-quit-all)])

(provide 'causal-calc-variables)
;;; causal-calc-variables.el ends here
