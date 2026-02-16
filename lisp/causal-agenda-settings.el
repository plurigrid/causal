;;; causal-agenda-settings.el --- Causal Agenda Settings -*- lexical-binding: t; -*-

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
(require 'org-agenda)
(require 'causal-lib)

(defun causal-agenda--customize-group ()
  "Customize Agenda group."
  (interactive)
  (customize-group "org-agenda"))

(defun causal-agenda-about-agenda ()
  "Causal Agenda is a Transient menu for Agenda.

Learn more about using Causal Agenda at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal Agenda, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal Agenda was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Causal Agenda.

Always choose love."
  (ignore))

(defun causal-agenda-about ()
  "About information for Causal Agenda."
  (interactive)
  (describe-function #'causal-agenda-about-agenda))


(transient-define-prefix causal-agenda-settings-tmenu ()
  "Settings."
  ["Modes"
   [("l" "Log" org-agenda-log-mode
     :description (lambda () (causal-lib-checkbox-label org-agenda-show-log "Log"))
     :transient t)
    ("G" "Grid" org-agenda-toggle-time-grid
     :description (lambda () (causal-lib-checkbox-label org-agenda-use-time-grid "Grid"))
     :transient t)]
   [("D" "Diary" org-agenda-toggle-diary
     :description (lambda () (causal-lib-checkbox-label org-agenda-include-diary "Diary"))
     :transient t)
    ("F" "Follow" org-agenda-follow-mode
     :description (lambda () (causal-lib-checkbox-label org-agenda-follow-mode "Follow"))
     :transient t)]
   [("R" "Clock Report" org-agenda-clockreport-mode
     :description (lambda () (causal-lib-checkbox-label org-agenda-clockreport-mode "Clock Report"))
     :transient t)
    ("E" "Entry" org-agenda-entry-text-mode
     :description (lambda () (causal-lib-checkbox-label org-agenda-entry-text-mode "Entry"))
     :transient t)]]

  ["Customize"
   [("f" "Agenda Files" causal-agenda-customize-agenda-files)
    ("d" causal-agenda-customize-agenda-include-diary
     :description (lambda ()
                    (causal-lib-checkbox-label org-agenda-include-diary "Include Diary")))
    ("i" causal-agenda-customize-agenda-include-inactive-timestamps
     :description (lambda ()
                    (causal-lib-checkbox-label org-agenda-include-inactive-timestamps "Include Inactive Timestamps")))

    ("x" causal-agenda-customize-agenda-include-deadlines
     :description (lambda ()
                    (causal-lib-checkbox-label org-agenda-include-deadlines "Include Deadlines")))

    ("m" causal-agenda-customize-ampm
     :description (lambda ()
                    (causal-lib-checkbox-label org-agenda-timegrid-use-ampm "Use AM/PM")))]

   [("I" "Next state on clock-in" causal-agenda-customize-org-clock-in-switch-to-state)
    ("O" "Next state on clock-out" causal-agenda-customize-org-clock-out-switch-to-state)
    ("," "Agenda Group" causal-agenda--customize-group)
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)]]

  [:class transient-row
          (causal-lib-quit-one)
          ("a" "About" causal-agenda-about :transient nil)

          (causal-lib-quit-all)])


(defun causal-agenda-customize-ampm ()
  "Customize variable `org-agenda-timegrid-use-ampm'."
  (interactive)
  (customize-variable 'org-agenda-timegrid-use-ampm))

(defun causal-agenda-customize-agenda-files ()
  "Customize variable `org-agenda-files'."
  (interactive)
  (customize-variable 'org-agenda-files))

(defun causal-agenda-customize-agenda-include-diary ()
  "Customize variable `org-agenda-include-diary'."
  (interactive)
  (customize-variable 'org-agenda-include-diary))

(defun causal-agenda-customize-agenda-include-deadlines ()
  "Customize variable `org-agenda-include-deadlines'."
  (interactive)
  (customize-variable 'org-agenda-include-deadlines))

(defun causal-agenda-customize-agenda-include-inactive-timestamps ()
  "Customize variable `org-agenda-include-inactive-timestamps'."
  (interactive)
  (customize-variable 'org-agenda-include-inactive-timestamps))

(defun causal-agenda-customize-org-clock-in-switch-to-state ()
  "Customize variable `org-clock-in-switch-to-state'."
  (interactive)
  (customize-variable 'org-clock-in-switch-to-state))

(defun causal-agenda-customize-org-clock-out-switch-to-state ()
  "Customize variable `org-clock-out-switch-to-state'."
  (interactive)
  (customize-variable 'org-clock-out-switch-to-state))

(provide 'causal-agenda-settings)
;;; causal-agenda-settings.el ends here
