;;; causal-org-settings.el --- Causal Org Settings -*- lexical-binding: t; -*-

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

;;; Code:
(require 'org)
(require 'causal-lib)


;; -------------------------------------------------------------------
;; Customize Functions

;; Groups
(defun causal-org--customize-group ()
  "Customize Org group."
  (interactive)
  (customize-group "org"))

(defun causal-org--customize-group-priorities ()
  "Customize Org Priorities group."
  (interactive)
  (customize-group "Org Priorities"))

(defun causal-org--customize-group-goto ()
  "Customize Org Goto group."
  (interactive)
  (customize-group "Org Goto"))


;; Customize Variables
(defun causal-org--customize-directory ()
  "Customize variable `org-directory'."
  (interactive)
  (customize-variable 'org-directory))

(defun causal-org--customize-default-notes-file ()
  "Customize variable `org-default-notes-file'."
  (interactive)
  (customize-variable 'org-default-notes-file))

(defun causal-org--customize-todo-keywords ()
  "Customize variable `org-todo-keywords'."
  (interactive)
  (customize-variable 'org-todo-keywords))

(defun causal-org--customize-refile-targets ()
  "Customize variable `org-refile-targets'."
  (interactive)
  (customize-variable 'org-refile-targets))

(defun causal-org--customize-imenu-depth ()
  "Customize variable `org-imenu-depth'."
  (interactive)
  (customize-variable 'org-imenu-depth))

(defun causal-org--customize-support-shift-select ()
  "Customize variable `org-support-shift-select'."
  (interactive)
  (customize-variable 'org-support-shift-select))

(defun causal-org--customize-startup-folded ()
  "Customize variable `org-startup-folded'."
  (interactive)
  (customize-variable 'org-startup-folded))

(defun causal-org--customize-startup-indented ()
  "Customize variable `org-startup-indented'."
  (interactive)
  (customize-variable 'org-startup-indented))

(defun causal-org--customize-show-notification-handler ()
  "Customize variable `org-show-notification-handler'."
  (interactive)
  (customize-variable 'org-show-notification-handler))

(defun causal-org--customize-log-done ()
  "Customize variable `org-log-done'."
  (interactive)
  (customize-variable 'org-log-done))

(defun causal-org--customize-insert-heading-respect-content ()
  "Customize variable `org-insert-heading-respect-content'."
  (interactive)
  (customize-variable 'org-insert-heading-respect-content))

(defun causal-org--customize-hide-leading-stars ()
  "Customize variable `causal-org-hide-leading-stars'."
  (interactive)
  (customize-variable 'org-hide-leading-stars))

(defun causal-org--customize-hide-emphasis-markers ()
  "Customize variable `org-hide-emphasis-markers'."
  (interactive)
  (customize-variable 'org-hide-emphasis-markers))

(defun causal-org--customize-use-speed-commands ()
  "Customize variable `org-use-speed-commands'."
  (interactive)
  (customize-variable 'org-use-speed-commands))

(defun causal-org--customize-yank-image-save-method ()
  "Customize variable `org-yank-image-save-method'."
  (interactive)
  (customize-variable 'org-yank-image-save-method))

(defun causal-org-about-org ()
  "Causal Org is a Transient menu for Org mode (`org-mode').

Learn more about using Causal Org at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal Org, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal Org was conceived and crafted by Charles Choi in San Francisco,
California.

Thank you for using Causal Org.

Always choose love."
  (ignore))

(defun causal-org-about ()
  "About information for Causal Org."
  (interactive)
  (describe-function #'causal-org-about-org))


;; -------------------------------------------------------------------
;; Transients
(transient-define-prefix causal-org-settings-tmenu ()
  "Causal Org settings menu."
  ["Causal Org Settings"

   [("h" "Heading & TODO›" causal-org-settings-heading-tmenu)
    ("f" "Files & Directories›" causal-org-settings-files-tmenu)
    ("d" "Display›" causal-org-settings-display-tmenu)]

   [("k" "Keyboard›" causal-org-settings-keyboard-tmenu)
    ("c" "Clocks›" causal-org-settings-clock-tmenu)
    ("y" "Copy & Paste›" causal-org-settings-killyank-tmenu)]

   [("G" "Org Group" causal-org--customize-group)]]

  ["General"
   :class transient-row
   (causal-lib-customize-unicode)
   (causal-lib-customize-hide-navigation)]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-org-about :transient nil)
   ("RET" "Done" transient-quit-all)
   (causal-lib-quit-all)])

(transient-define-prefix causal-org-settings-heading-tmenu ()
  "Customize Org heading settings."
  ["Causal Org Settings: Heading"
   ["TODO"
    ("k" "TODO Keywords" causal-org--customize-todo-keywords)
    ("l" "Log Done" causal-org--customize-log-done)
    ("p" "Priorities" causal-org--customize-group-priorities)]

   ["Heading"
    ("i" "Index Menu Depth" causal-org--customize-imenu-depth
     :description (lambda () (format "imenu Depth (%d)" org-imenu-depth)))
    ("r" "Insertion Style" causal-org--customize-insert-heading-respect-content)]

   ["Goto"
    ("g" "Goto" causal-org--customize-group-goto)]]

  causal-lib-navigation-group-with-return)

(transient-define-prefix causal-org-settings-files-tmenu ()
  "Customize Org file and directory settings."
  ["Causal Org Settings: Files & Directories"
   ("o" "Default Directory" causal-org--customize-directory
    :description (lambda () (format "Default Directory (%s)" org-directory)))
   ("n" "Default Notes File" causal-org--customize-default-notes-file
    :description (lambda () (format "Default Notes File (%s)" org-default-notes-file)))
   ("r" "Refile Targets" causal-org--customize-refile-targets)]
  causal-lib-navigation-group-with-return)

(transient-define-prefix causal-org-settings-display-tmenu ()
  "Customize Org display settings. ."
  ["Causal Org Settings: Display"
   ["Hide"
    ("s" "Leading Stars" causal-org--customize-hide-leading-stars
     :description (lambda () (causal-lib-checkbox-label org-hide-leading-stars
                                                   "Hide Leading Stars")))
    ("e" "Emphasis Markers" causal-org--customize-hide-emphasis-markers
     :description (lambda () (causal-lib-checkbox-label org-hide-emphasis-markers
                                                   "Hide Emphasis Markers")))]
   ["Startup"
    ("f" "Folded" causal-org--customize-startup-folded
     :description (lambda () (format "Startup Folded (%s)" org-startup-folded)))

    ("i" "Indented" causal-org--customize-startup-indented
     :description (lambda () (format "Startup Indented (%s)" org-startup-indented)))]]
  causal-lib-navigation-group-with-return)

(transient-define-prefix causal-org-settings-keyboard-tmenu ()
  "Customize Org keyboard settings."
  ["Causal Org Settings: Keyboard"
   ("s" "Support Shift Select" causal-org--customize-support-shift-select
    :description (lambda () (causal-lib-checkbox-label org-support-shift-select
                                                  "Shift Select")))
   ("S" "Use Speed Commands" causal-org--customize-use-speed-commands
    :description (lambda () (causal-lib-checkbox-label org-use-speed-commands
                                                  "Speed Commands")))]
  causal-lib-navigation-group-with-return)

(transient-define-prefix causal-org-settings-clock-tmenu ()
  "Customize Org clock settings."
  ["Causal Org Settings: Clock"
   ("n" "Notification Handler" causal-org--customize-show-notification-handler)]
  causal-lib-navigation-group-with-return)

(transient-define-prefix causal-org-settings-killyank-tmenu ()
  "Customize Org copy & paste (kill & yank) settings."
  ["Causal Org Settings: Copy & Paste (Kill & Yank)"
   ("y" "Yank Image Save Method" causal-org--customize-yank-image-save-method)]
  causal-lib-navigation-group-with-return)

(provide 'causal-org-settings)
;;; causal-org-settings.el ends here
