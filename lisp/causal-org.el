;;; causal-org.el --- Transient UI for Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charles Y. Choi

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

;; This library provides a Transient-based user interface for `org-mode'.

;; INSTALLATION

;; In your initialization file, bind the Transient `causal-org-tmenu' to your
;; key binding of preference.

;; (require 'causal-org) ; optional if using autoloaded menu
;; (keymap-set org-mode-map "M-m" #'causal-org-tmenu)
;; (keymap-set org-table-fedit-map "M-m" #'causal-org-table-fedit-tmenu)

;;; Code:
(require 'org)
(require 'causal-org-settings)
(require 'causal-org-utils)

;;;###autoload (autoload 'causal-org-tmenu "causal-org" nil t)
(transient-define-prefix causal-org-tmenu ()
  "Main menu for Causal Org.

Top level menu for Causal Org. The menu offering is context-dependent on
where the point is located in an Org (`org-mode') document.

Causal Org is opinionated in that it endeavors to provide an effective
set of commands based on what type of Org section the point is in. Major
sections supported by this menu include:

- Heading
- Item
- Table
- Block

This menu does not make effort to provide exhaustive coverage of all
possible Org commands.

While this menu is raised, point navigation is supported using standard
Emacs key bindings for movement."

  :refresh-suffixes t
  :transient-non-suffix t

  ;; Context-Specific
  causal-org-heading-group
  causal-org-item-group
  causal-org-table-group
  causal-org-keyword-group
  causal-org-body-group
  causal-org-block-group

  ;; Common
  causal-org-utility-group
  causal-org-navigation-group

  [:class transient-row
   :if causal-org-mode-p
   (causal-lib-quit-one)
   ("," "Settings›" causal-org-settings-tmenu)
   ("I" "ⓘ" causal-org-info
    :description (lambda () (causal-org-unicode-get :info)))
   ("U" "Undo" undo :transient t)
   ("RET" "Done" transient-quit-all)
   (causal-lib-quit-all)])


;;;###autoload (autoload 'causal-org-table-fedit-tmenu "causal-org" nil t)
(transient-define-prefix causal-org-table-fedit-tmenu ()
  "Menu for Org table formula editing.

This menu provides commands for inserting formula references."
  :transient-non-suffix t

  ["Org Table Formula Edit"
   :pad-keys t
   ["Row"
    ("@<" "⤒ First" causal-org-table-fedit-first-row-reference
     :description (lambda () (causal-org-unicode-get :first-row)))
    ("@>" "⤓ Last" causal-org-table-fedit-last-row-reference
     :description (lambda () (causal-org-unicode-get :last-row)))]

   ["Column"
    ("$<" "⇤ First" causal-org-table-fedit-first-column-reference
     :description (lambda () (causal-org-unicode-get :first-column)))
    ("$>" "⇥ Last" causal-org-table-fedit-last-column-reference
     :description (lambda () (causal-org-unicode-get :last-column)))]

   ["H Line (-)"
    ("1" "First (@I)" causal-org-table-fedit-first-hline-reference)
    ("2" "Second (@II)" causal-org-table-fedit-second-hline-reference)
    ("r" "Range (@I..@II)" causal-org-table-fedit-hline-range-reference)]

   ["Vector"
    ("s" "sum" causal-org-table-insert-calc-sum)
    ("m" "mean" causal-org-table-insert-calc-mean)
    ("a" "max" causal-org-table-insert-calc-max)
    ("z" "min" causal-org-table-insert-calc-min)]

   ["Info"
    :description (lambda () (causal-org-unicode-get :info))
    ("F" "Formula Syntax" causal-org-table-info-formula-syntax)
    ("R" "References" causal-org-table-info-references)
    ("f" "𝑓(𝑥)" causal-org-table-info-calc-functions)]]

  causal-lib-navigation-group-with-undo-and-return)

(provide 'causal-org)
;;; causal-org.el ends here
