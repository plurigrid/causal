;;; causal-calendar.el --- Transient UI for Calendar -*- lexical-binding: t; -*-

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

;; Causal Calendar is an opinionated Transient-based user interface for the
;; Emacs regular expression editor.

;; INSTALLATION
;; (require 'causal-calendar) ; optional if using autoloaded menu
;; (keymap-set calendar-mode-map "C-o" #'causal-calendar)

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:
(require 'causal-calendar-utils)
(require 'causal-calendar-settings)

;;;###autoload (autoload 'causal-calendar "causal-calendar" nil t)
(defun causal-calendar ()
  "Call Causal Calendar main menu.

Helper function for calling `causal-calendar-tmenu'. Calls
`calendar-redraw' to fix window sizing."
  (interactive)
  (call-interactively #'causal-calendar-tmenu)
  (calendar-redraw))

;;;###autoload (autoload 'causal-calendar-tmenu "causal-calendar" nil t)
(transient-define-prefix causal-calendar-tmenu ()
  "Transient menu for Calendar commands.

Main menu for `calendar' commands.

* References
- Info node `(emacs) Calendar/Diary'"
  causal-calendar--navigation-group

  [["Conversions"
    ("c" "Conversions›" causal-calendar-conversions-tmenu)
    ("A" "Convert to all" calendar-print-other-dates :transient t)
    ("i" "ISO Date" calendar-iso-print-date :transient t)]

   ["Holidays"
    ("H" "Holidays in span" calendar-list-holidays :transient t)
    ("h" "Holidays at point" calendar-cursor-holidays :transient t)
    ("x" "Mark Holidays" calendar-mark-holidays :transient t)
    ("u" "Unmark" calendar-unmark :transient t)]

   ["Misc"
    ("O" "Org Agenda" org-calendar-goto-agenda)
    ("d" "Diary" diary-view-entries :transient t)
    ("s" "All Diary" diary-show-all-entries :transient t)
    ("D" "Diary & Goto›" causal-calendar-diary-and-goto-tmenu)]

   ["Almanac"
    :pad-keys t
    ("M" "Lunar Phases" calendar-lunar-phases
     :description (lambda () (causal-calendar-unicode-get :lunar))
     :transient t)
    ("S" "Sunrise/Sunset" calendar-sunrise-sunset
     :description (lambda () (causal-calendar-unicode-get :sunrise))
     :transient t)
    ("M-m" "Sunrise/Sunset Month" calendar-sunrise-sunset-month
     :description (lambda () (format "%s Month" (causal-calendar-unicode-get :sunrise)))
     :transient t)]]

  ["Region"
    :class transient-row
    ("C-SPC" "Set Mark" calendar-set-mark :transient t)
    ("=" "Count Days" calendar-count-days-region :transient t)]

  [:class transient-row
   (causal-lib-quit-one)
   ("," "Settings›" causal-calendar-settings-tmenu)
   ("I" "ⓘ" (lambda ()
              (interactive)
              (calendar-exit)
              (calendar-goto-info-node)))
   ("RET" "Done" transient-quit-all)
   ("q" "Quit" calendar-exit)])

(provide 'causal-calendar)
;;; causal-calendar.el ends here
