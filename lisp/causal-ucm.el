;;; causal-ucm.el --- Transient UI for Unison Codebase Manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, languages

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

;; This library provides a Transient-based user interface for the Unison
;; Codebase Manager (UCM).  UCM is the CLI for the Unison programming
;; language, which uses content-addressed code storage.
;;
;; INSTALLATION
;;
;; (require 'causal-ucm)
;; (keymap-set unison-ts-mode-map "C-o" #'causal-ucm-tmenu)
;;
;; Or if you use a comint-based UCM buffer:
;; (keymap-set comint-mode-map "C-o" #'causal-ucm-tmenu)

;;; Code:

(require 'causal-ucm-settings)
(require 'causal-ucm-utils)
(require 'causal-ucm-scratch)

;;;###autoload (autoload 'causal-ucm-tmenu "causal-ucm" nil t)
(transient-define-prefix causal-ucm-tmenu ()
  "Transient menu for the Unison Codebase Manager."
  :refresh-suffixes t
  ["Causal UCM"
   ["Codebase"
    :pad-keys t
    ("a" "Add" causal-ucm-add
     :description (lambda () (causal-ucm-unicode-get :add)))
    ("u" "Update" causal-ucm-update
     :description (lambda () (causal-ucm-unicode-get :update)))
    ("d" "Delete" causal-ucm-delete
     :description (lambda () (causal-ucm-unicode-get :delete)))
    ("l" "Load" causal-ucm-load)
    ("D" "Diff update" causal-ucm-diff-update)]

   ["Browse"
    :pad-keys t
    ("v" "View" causal-ucm-view
     :description (lambda () (causal-ucm-unicode-get :view)))
    ("e" "Edit" causal-ucm-edit
     :description (lambda () (causal-ucm-unicode-get :edit)))
    ("f" "Find" causal-ucm-find
     :description (lambda () (causal-ucm-unicode-get :find)))
    ("F" "Find all" causal-ucm-find-all)
    ("L" "List (ls)" causal-ucm-ls
     :description (lambda () (causal-ucm-unicode-get :ls)))
    ("o" "Docs" causal-ucm-docs
     :description (lambda () (causal-ucm-unicode-get :docs)))]

   ["Dependencies"
    :pad-keys t
    ("p" "Dependencies" causal-ucm-dependencies)
    ("P" "Dependents" causal-ucm-dependents)
    ("t" "Todo" causal-ucm-todo
     :description (lambda () (causal-ucm-unicode-get :todo)))]]

  ["Branching & Projects"
   ["Branch"
    :pad-keys t
    ("b" "Branch" causal-ucm-branch
     :description (lambda () (causal-ucm-unicode-get :branch)))
    ("B" "Branches" causal-ucm-branches)
    ("s" "Switch" causal-ucm-switch
     :description (lambda () (causal-ucm-unicode-get :switch)))
    ("m" "Merge" causal-ucm-merge
     :description (lambda () (causal-ucm-unicode-get :merge)))
    ("M" "Merge commit" causal-ucm-merge-commit)
    ("k" "Back" causal-ucm-back)]

   ["Project"
    :pad-keys t
    ("c" "Create project" causal-ucm-project-create
     :description (lambda () (causal-ucm-unicode-get :project)))
    ("j" "Projects" causal-ucm-projects)]

   ["Remote"
    :pad-keys t
    ("C" "Clone" causal-ucm-clone
     :description (lambda () (causal-ucm-unicode-get :clone)))
    (">" "Push" causal-ucm-push
     :description (lambda () (causal-ucm-unicode-get :push)))
    ("<" "Pull" causal-ucm-pull
     :description (lambda () (causal-ucm-unicode-get :pull)))
    ("i" "Install lib" causal-ucm-lib-install
     :description (lambda () (causal-ucm-unicode-get :lib)))
    ("A" "Auth login" causal-ucm-auth-login)]]

  ["Testing & Running"
   ["Test"
    :pad-keys t
    ("T" "Test" causal-ucm-test
     :description (lambda () (causal-ucm-unicode-get :test)))
    ("I" "IO test" causal-ucm-io-test)
    ("!" "Test all" causal-ucm-test-all)]

   ["Run"
    :pad-keys t
    ("r" "Run" causal-ucm-run
     :description (lambda () (causal-ucm-unicode-get :run)))
    ("x" "Compile" causal-ucm-compile)
    ("U" "UI" causal-ucm-ui)]

   ["History"
    :pad-keys t
    ("h" "History" causal-ucm-history
     :description (lambda () (causal-ucm-unicode-get :history)))
    ("z" "Undo" causal-ucm-undo
     :description (lambda () (causal-ucm-unicode-get :undo)))
    ("R" "Reset" causal-ucm-reset)
    ("g" "Reflog" causal-ucm-reflog)]]

  ["Scratch Pads"
   ["Open"
    :pad-keys t
    ("N" "New definition" causal-ucm-scratch-new)
    ("E" "Edit term…" causal-ucm-scratch-term)
    ("W" "Edit namespace…" causal-ucm-scratch-namespace)
    ("X" "Explore term…" causal-ucm-scratch-explore)
    ("K" "Test term…" causal-ucm-scratch-test)
    ("J" "Merge branch…" causal-ucm-scratch-merge)]

   ["Utilities"
    :pad-keys t
    ("n" "Move/Rename" causal-ucm-move)
    ("w" "Alias term" causal-ucm-alias-term)
    ("G" "Upgrade dep" causal-ucm-upgrade)
    ("V" "Version" causal-ucm-version)
    ("?" "Help cmd" causal-ucm-help)
    ("S" "Start UCM" causal-ucm-start)]]

  [:class transient-row
   (causal-lib-quit-one)
   (",," "Settings" causal-ucm-settings-tmenu)
   (causal-lib-quit-all)])

(provide 'causal-ucm)
;;; causal-ucm.el ends here
