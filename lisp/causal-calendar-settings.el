;;; causal-calendar-settings.el --- Causal Calendar Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Charles Y. Choi

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
(require 'solar)
(require 'org-agenda)
(require 'causal-calendar-constants)

(transient-define-prefix causal-calendar-settings-tmenu ()
  "Causal Calendar & Diary settings menu.

Customize settings for Calendar and Diary modes."

  ["Calendar"
   ["Customize"
    ("C" "Calendar Group" causal-calendar--customize-calendar-group)
    ("H" "Mark Holidays" causal-calendar--customize-calendar-mark-holidays-flag
     :description (lambda ()
                    (causal-lib-checkbox-label
                     calendar-mark-holidays-flag
                     "Mark Holidays")))
    ("E" "Mark Diary Entries" causal-calendar--customize-calendar-mark-diary-entries-flag
     :description (lambda ()
                    (causal-lib-checkbox-label
                     calendar-mark-diary-entries-flag
                     "Mark Diary Entries")))]

   ["Hooks"
    ("v" "Move Hook" causal-calendar--customize-calendar-move-hook)]

   ["Location"
    ("N" "Location Name" causal-calendar--customize-calendar-location-name)
    ("A" "Latitude" causal-calendar--customize-calendar-latitude)
    ("O" "Longitude" causal-calendar--customize-calendar-longitude)]]

  ["Diary"
   ["Customize"
    ("D" "Diary Group" causal-calendar--customize-diary-group)]

   ["Hooks"
    ("l" "List" causal-calendar--customize-diary-list-entries-hook)
    ("m" "Mark" causal-calendar--customize-diary-mark-entries-hook)]

   ["Non-Gregorian Hooks"
    ("L" "List" causal-calendar--customize-diary-nongregorian-listing-hook)
    ("M" "Mark" causal-calendar--customize-diary-nongregorian-marking-hook)]

   ["Org Agenda"
    ("d" "Include Diary" causal-calendar--customize-org-agenda-include-diary
     :description (lambda ()
                    (causal-lib-checkbox-label
                     org-agenda-include-diary
                     "Include Diary")))]]

  ["General"
   (causal-lib-customize-unicode)
   (causal-lib-customize-hide-navigation)]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-calendar-about :transient nil)
   ("RET" "Done" transient-quit-all)
   (causal-lib-quit-all)])

(defun causal-calendar--customize-calendar-group ()
  "Customize calendar group."
  (interactive)
  (customize-group "calendar"))

(defun causal-calendar--customize-diary-group ()
  "Customize diary group."
  (interactive)
  (customize-group "diary"))

(defun causal-calendar--customize-org-agenda-include-diary ()
  "Customize variable `org-agenda-include-diary'."
  (interactive)
  (customize-variable 'org-agenda-include-diary))

(defun causal-calendar--customize-diary-list-entries-hook ()
  "Customize variable `diary-list-entries-hook'."
  (interactive)
  (customize-variable 'diary-list-entries-hook))

(defun causal-calendar--customize-diary-mark-entries-hook ()
  "Customize variable `diary-mark-entries-hook'."
  (interactive)
  (customize-variable 'diary-mark-entries-hook))

(defun causal-calendar--customize-diary-nongregorian-listing-hook ()
  "Customize variable `diary-nongregorian-listing-hook'."
  (interactive)
  (customize-variable 'diary-nongregorian-listing-hook))

(defun causal-calendar--customize-diary-nongregorian-marking-hook ()
  "Customize variable `diary-nongregorian-marking-hook'."
  (interactive)
  (customize-variable 'diary-nongregorian-marking-hook))

(defun causal-calendar--customize-calendar-mark-holidays-flag ()
  "Customize variable `calendar-mark-holidays-flag'."
  (interactive)
  (customize-variable 'calendar-mark-holidays-flag))

(defun causal-calendar--customize-calendar-mark-diary-entries-flag ()
  "Customize variable `calendar-mark-diary-entries-flag'."
  (interactive)
  (customize-variable 'calendar-mark-diary-entries-flag))

(defun causal-calendar--customize-calendar-location-name ()
  "Customize variable `calendar-location-name'."
  (interactive)
  (customize-variable 'calendar-location-name))

(defun causal-calendar--customize-calendar-latitude ()
  "Customize variable `calendar-latitude'."
  (interactive)
  (customize-variable 'calendar-latitude))

(defun causal-calendar--customize-calendar-longitude ()
  "Customize variable `calendar-longitude'."
  (interactive)
  (customize-variable 'calendar-longitude))

(defun causal-calendar--customize-calendar-move-hook ()
  "Customize variable `calendar-move-hook'."
  (interactive)
  (customize-variable 'calendar-move-hook))


(defun causal-calendar-about-calendar ()
  "Causal Calendar is a Transient menu for Calendar.

Learn more about using Causal Calendar at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal Calendar, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal Calendar was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Causal Calendar.

Always choose love."
  (ignore))

(defun causal-calendar-about ()
  "About information for Causal Calendar."
  (interactive)
  (describe-function #'causal-calendar-about-calendar))

(provide 'causal-calendar-settings)
;;; causal-calendar-settings.el ends here
