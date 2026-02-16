;;; causal-agenda.el --- Transient UI for Agenda -*- lexical-binding: t; -*-

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

;; Causal Agenda is an opinionated Transient user interface for Org Agenda.

;; INSTALLATION
;; (require 'causal-agenda) ; optional if using autoloaded menu
;; (keymap-set org-agenda-mode-map "C-o" #'causal-agenda-tmenu)
;; (keymap-set org-agenda-mode-map "M-j" #'org-agenda-clock-goto) ; optional
;; (keymap-set org-agenda-mode-map "J" #'bookmark-jump) ; optional

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:
(require 'org-agenda)
(require 'bookmark)
(require 'causal-agenda-utils)
(require 'causal-agenda-settings)

;;;###autoload (autoload 'causal-agenda-tmenu "causal-agenda" nil t)
(transient-define-prefix causal-agenda-tmenu ()
  "Transient menu for Org Agenda."
  :refresh-suffixes t
  ["Agenda"
   :class transient-row
   ("d" "Day" org-agenda-day-view
    :inapt-if-not causal-agenda-type-agendap
    :transient t)
   ("w" "Week" org-agenda-week-view
    :inapt-if-not causal-agenda-type-agendap
    :transient t)
   ("t" "Fortnight" org-agenda-fortnight-view
    :inapt-if-not causal-agenda-type-agendap
    :transient t)
   ("m" "Month" org-agenda-month-view
    :inapt-if-not causal-agenda-type-agendap
    :transient t)
   ("y" "Year" org-agenda-year-view
    :inapt-if-not causal-agenda-type-agendap
    :transient t)
   ("." "Now" causal-agenda-goto-now :transient t)]

  ["Filter"
   [("/" "Filter…" org-agenda-filter :transient t)
    ("=" "Regexp…" org-agenda-filter-by-regexp :transient t)]

   [("\\" "Tag…" org-agenda-filter-by-tag :transient t)
    ("^" "Headline…" org-agenda-filter-by-top-headline
     :inapt-if-not (lambda () (causal-agenda-headlinep))
     :transient t)]

   [("<" "Category…" org-agenda-filter-by-category
     :inapt-if-not (lambda () (causal-agenda-headlinep))
     :transient t)
    ("_" "Effort…" org-agenda-filter-by-effort :transient t)]

   [("|" "Remove all" org-agenda-filter-remove-all :transient t)]]


  ["Actions"
   :class transient-row
   ("o" "Operations›" causal-agenda-operations-tmenu)
   ("M" "Mark›" causal-agenda-mark-tmenu
    :inapt-if-not (lambda () (causal-agenda-headlinep)))
   ("s" "Save all" org-save-all-org-buffers :transient t)
   ("k" "Capture…" org-capture)
   ("a" "Agenda…" org-agenda)]

  causal-agenda-agenda-navigation-group

  ["Utils"
   :class transient-row
   (";" "⏱️" org-timer-set-timer
    :description (lambda () (format "%s…" (causal-agenda-unicode-get :timer)))
    :transient t)
   ("c" "📅" org-agenda-goto-calendar
    :inapt-if-not causal-agenda-type-agendap
    :description (lambda () (format "%s" (causal-agenda-unicode-get :date))))
   ("l" "Almanac›" causal-agenda-almanac-tmenu
    :inapt-if-not causal-agenda-type-datep)
   ("J" "Bookmark jump…" bookmark-jump
    :description (lambda () (format "%s…"
                                    (causal-agenda-unicode-get :jumpbookmark))))]
  [:class transient-row
   (causal-lib-quit-one)
   ("RET" "Open" org-agenda-switch-to)
   ("C-/" "Undo" org-agenda-undo)
   ("I" "ⓘ Info" org-info-find-node)
   ("," "Settings›" causal-agenda-settings-tmenu)
   ("q" "Quit" org-agenda-quit)])

(transient-define-prefix causal-agenda-almanac-tmenu ()
  "Almanac menu."
  :refresh-suffixes t
  ["Almanac"
   :class transient-row
   ("S" "🌅" org-agenda-sunrise-sunset
    :inapt-if-not causal-agenda-type-datep
    :description (lambda () (format "%s" (causal-agenda-unicode-get :sunrise)))
    :transient t)
   ("M" "🌙" org-agenda-phases-of-moon
    :inapt-if-not causal-agenda-type-datep
    :description (lambda () (format "%s" (causal-agenda-unicode-get :lunar))))
   ("H" "Holidays" org-agenda-holidays
    :inapt-if-not causal-agenda-type-datep)]

  causal-agenda-agenda-navigation-group
  causal-agenda-navigation-group)


(transient-define-prefix causal-agenda-operations-tmenu ()
  :refresh-suffixes t
  ["Operations"
   :pad-keys t
   :inapt-if-not (lambda () (causal-agenda-headlinep))
   [("t" "Todo…" org-agenda-todo :transient t)
    (":" "Tags…" org-agenda-set-tags :transient t)
    ("B" "Bulk Action…" org-agenda-bulk-action :transient t)]
   [("s" "Schedule…" org-agenda-schedule :transient t)
    ("d" "Deadline…" org-agenda-deadline :transient t)]
   [("+" "↑ Priority" org-agenda-priority-up
     :description (lambda () (format "%s Priority" (causal-agenda-unicode-get :up)))
     :transient t)
    ("-" "↓ Priority" org-agenda-priority-down
     :description (lambda () (format "%s Priority" (causal-agenda-unicode-get :down)))
     :transient t)]
   [("R" "Refile…" org-agenda-refile)
    ("z" "Add Note" org-agenda-add-note)]
   [("S" "Set Property…" org-agenda-set-property)
    ("A" "Archive…" org-agenda-archive-default-with-confirmation)]]

  ["Clock"
   :class transient-row
   ("I" "Clock In" causal-agenda-clock-in
    :inapt-if-not (lambda () (causal-agenda-headlinep))
    :transient t)
   ("O" "Clock Out" causal-agenda-clock-out
    :inapt-if-not org-clocking-p
    :transient t)
   ("x" "Cancel" causal-agenda-clock-cancel
    :inapt-if-not org-clocking-p
    :transient t)
   ("m" "Modify" org-clock-modify-effort-estimate
    :inapt-if-not org-clocking-p
    :transient t)]

  causal-agenda-agenda-navigation-group
  causal-agenda-navigation-group)

(transient-define-prefix causal-agenda-mark-tmenu ()
  ["Mark"
    :pad-keys t
    [("m" "Mark" org-agenda-bulk-mark :transient t)
     ("x" "Mark Regexp…" org-agenda-bulk-mark-regexp :transient t)]
    [("u" "Unmark" org-agenda-bulk-unmark :transient t)
     ("U" "Unmark" org-agenda-bulk-unmark-all :transient t)]
    [("t" "Toggle" org-agenda-bulk-toggle :transient t)
     ("T" "Toggle all" org-agenda-bulk-toggle-all :transient t)]
    [("B" "Bulk Action…" org-agenda-bulk-action :transient t)]]

  causal-agenda-agenda-navigation-group
  causal-agenda-navigation-group)

(provide 'causal-agenda)
;;; causal-agenda.el ends here
