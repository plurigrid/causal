;;; causal-ucm-utils.el --- Causal UCM Utils  -*- lexical-binding: t; -*-

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

;; Utility functions for Causal UCM, a Transient-based porcelain for
;; the Unison Codebase Manager (UCM).
;;
;; UCM commands are sent to a running UCM process via comint or
;; dispatched as shell commands when no live session exists.

;;; Code:

(require 'comint)
(require 'causal-lib)

(defconst causal-ucm-unicode-db
  '((:add        . '("⊕" "Add"))
    (:update     . '("⟳" "Update"))
    (:delete     . '("⊖" "Delete"))
    (:find       . '("🔍" "Find"))
    (:view       . '("👁" "View"))
    (:edit       . '("✎" "Edit"))
    (:branch     . '("⑂" "Branch"))
    (:merge      . '("⊔" "Merge"))
    (:switch     . '("⇌" "Switch"))
    (:push       . '("↑" "Push"))
    (:pull       . '("↓" "Pull"))
    (:test       . '("✓" "Test"))
    (:run        . '("▶" "Run"))
    (:project    . '("◆" "Project"))
    (:history    . '("⏳" "History"))
    (:undo       . '("↺" "Undo"))
    (:ls         . '("≡" "List"))
    (:todo       . '("☐" "Todo"))
    (:docs       . '("📖" "Docs"))
    (:clone      . '("⊞" "Clone"))
    (:lib        . '("📦" "Lib")))
  "Unicode symbol DB for UCM Transient menus.")

(defun causal-ucm-unicode-get (key)
  "Lookup Unicode symbol for KEY in `causal-ucm-unicode-db'.

If `causal-lib-use-unicode' is non-nil, the Unicode symbol is
returned, otherwise a plain ASCII-range string."
  (causal-lib-unicode-db-get key causal-ucm-unicode-db))

;;; Process Management

(defvar causal-ucm--process-buffer "*ucm*"
  "Buffer name for the UCM comint process.")

(defun causal-ucm--live-p ()
  "Return non-nil if a UCM process is running."
  (and (get-buffer causal-ucm--process-buffer)
       (comint-check-proc causal-ucm--process-buffer)))

(defun causal-ucm-start ()
  "Start a UCM session in a comint buffer."
  (interactive)
  (unless (causal-ucm--live-p)
    (let ((buf (make-comint-in-buffer "ucm" causal-ucm--process-buffer
                                      (causal-ucm--executable) nil)))
      (with-current-buffer buf
        (setq-local comint-prompt-regexp "^[^ >\n]*> *"))
      (display-buffer buf)))
  (pop-to-buffer causal-ucm--process-buffer))

(defun causal-ucm--executable ()
  "Return the UCM executable path."
  (or (executable-find (bound-and-true-p causal-ucm-executable))
      (executable-find "ucm")
      (error "UCM executable not found; set `causal-ucm-executable'")))

(defun causal-ucm--send (command)
  "Send COMMAND string to the running UCM process."
  (if (causal-ucm--live-p)
      (with-current-buffer causal-ucm--process-buffer
        (goto-char (point-max))
        (insert command)
        (comint-send-input))
    (causal-ucm-start)
    (run-at-time 0.5 nil #'causal-ucm--send command)))

(defun causal-ucm--send-with-arg (base-cmd prompt)
  "Send BASE-CMD with argument read from minibuffer using PROMPT."
  (let ((arg (read-string prompt)))
    (causal-ucm--send (if (string-empty-p arg)
                          base-cmd
                        (format "%s %s" base-cmd arg)))))

;;; Interactive Commands — Codebase

(defun causal-ucm-add ()
  "UCM: add definitions from scratch file."
  (interactive)
  (causal-ucm--send-with-arg "add" "Add (term, blank for all): "))

(defun causal-ucm-update ()
  "UCM: update definitions from scratch file."
  (interactive)
  (causal-ucm--send-with-arg "update" "Update (term, blank for all): "))

(defun causal-ucm-delete ()
  "UCM: delete a term or type."
  (interactive)
  (causal-ucm--send-with-arg "delete" "Delete: "))

(defun causal-ucm-view ()
  "UCM: view source of a term or type."
  (interactive)
  (causal-ucm--send-with-arg "view" "View: "))

(defun causal-ucm-display ()
  "UCM: display rendered term."
  (interactive)
  (causal-ucm--send-with-arg "display" "Display: "))

(defun causal-ucm-docs ()
  "UCM: show docs for a term."
  (interactive)
  (causal-ucm--send-with-arg "docs" "Docs for: "))

(defun causal-ucm-edit ()
  "UCM: bring definition into scratch file."
  (interactive)
  (causal-ucm--send-with-arg "edit" "Edit: "))

(defun causal-ucm-find ()
  "UCM: search for definitions."
  (interactive)
  (causal-ucm--send-with-arg "find" "Find: "))

(defun causal-ucm-find-all ()
  "UCM: search including lib namespace."
  (interactive)
  (causal-ucm--send-with-arg "find.all" "Find all: "))

(defun causal-ucm-ls ()
  "UCM: list terms in namespace."
  (interactive)
  (causal-ucm--send-with-arg "ls" "List (namespace, blank for current): "))

(defun causal-ucm-todo ()
  "UCM: list outstanding issues."
  (interactive)
  (causal-ucm--send "todo"))

(defun causal-ucm-load ()
  "UCM: load a scratch file."
  (interactive)
  (causal-ucm--send-with-arg "load" "Load file (blank for default): "))

(defun causal-ucm-dependencies ()
  "UCM: list dependencies of a definition."
  (interactive)
  (causal-ucm--send-with-arg "dependencies" "Dependencies of: "))

(defun causal-ucm-dependents ()
  "UCM: list dependents of a definition."
  (interactive)
  (causal-ucm--send-with-arg "dependents" "Dependents of: "))

;;; Interactive Commands — Branching & Projects

(defun causal-ucm-branch ()
  "UCM: fork current branch."
  (interactive)
  (causal-ucm--send-with-arg "branch" "New branch name: "))

(defun causal-ucm-branches ()
  "UCM: list branches."
  (interactive)
  (causal-ucm--send "branches"))

(defun causal-ucm-switch ()
  "UCM: switch project or branch."
  (interactive)
  (causal-ucm--send-with-arg "switch" "Switch to (project or /branch): "))

(defun causal-ucm-merge ()
  "UCM: merge branch."
  (interactive)
  (causal-ucm--send-with-arg "merge" "Merge from: "))

(defun causal-ucm-merge-commit ()
  "UCM: merge.commit temporary branch back."
  (interactive)
  (causal-ucm--send "merge.commit"))

(defun causal-ucm-project-create ()
  "UCM: create a new project."
  (interactive)
  (causal-ucm--send-with-arg "project.create" "Project name: "))

(defun causal-ucm-projects ()
  "UCM: list projects."
  (interactive)
  (causal-ucm--send "projects"))

;;; Interactive Commands — Remote

(defun causal-ucm-clone ()
  "UCM: clone from Unison Share."
  (interactive)
  (causal-ucm--send-with-arg "clone" "Clone (e.g. @unison/json): "))

(defun causal-ucm-pull ()
  "UCM: pull from remote."
  (interactive)
  (causal-ucm--send-with-arg "pull" "Pull from: "))

(defun causal-ucm-push ()
  "UCM: push to remote."
  (interactive)
  (causal-ucm--send-with-arg "push" "Push to: "))

(defun causal-ucm-lib-install ()
  "UCM: install a library from Unison Share."
  (interactive)
  (causal-ucm--send-with-arg "lib.install" "Install lib (e.g. @unison/base): "))

(defun causal-ucm-auth-login ()
  "UCM: authenticate with Unison Share."
  (interactive)
  (causal-ucm--send "auth.login"))

;;; Interactive Commands — Testing & Running

(defun causal-ucm-test ()
  "UCM: run tests."
  (interactive)
  (causal-ucm--send-with-arg "test" "Test (namespace, blank for all): "))

(defun causal-ucm-test-all ()
  "UCM: run all tests including lib."
  (interactive)
  (causal-ucm--send "test.all"))

(defun causal-ucm-io-test ()
  "UCM: run a single IO test."
  (interactive)
  (causal-ucm--send-with-arg "io.test" "IO test: "))

(defun causal-ucm-run ()
  "UCM: run a program."
  (interactive)
  (causal-ucm--send-with-arg "run" "Run: "))

;;; Interactive Commands — History & Navigation

(defun causal-ucm-history ()
  "UCM: show branch history."
  (interactive)
  (causal-ucm--send "history"))

(defun causal-ucm-undo ()
  "UCM: undo last change."
  (interactive)
  (causal-ucm--send "undo"))

(defun causal-ucm-reset ()
  "UCM: reset branch to a hash."
  (interactive)
  (causal-ucm--send-with-arg "reset" "Reset to (hash or number): "))

(defun causal-ucm-reflog ()
  "UCM: show reflog."
  (interactive)
  (causal-ucm--send "reflog"))

(defun causal-ucm-back ()
  "UCM: undo last switch."
  (interactive)
  (causal-ucm--send "back"))

(defun causal-ucm-diff-update ()
  "UCM: preview what update would change."
  (interactive)
  (causal-ucm--send "diff.update"))

;;; Interactive Commands — Utility

(defun causal-ucm-ui ()
  "UCM: open local codebase UI."
  (interactive)
  (causal-ucm--send "ui"))

(defun causal-ucm-version ()
  "UCM: show version."
  (interactive)
  (causal-ucm--send "version"))

(defun causal-ucm-help ()
  "UCM: show help for a command."
  (interactive)
  (causal-ucm--send-with-arg "help" "Help for command: "))

(defun causal-ucm-move ()
  "UCM: rename a term, type, or namespace."
  (interactive)
  (let ((old (read-string "Move from: "))
        (new (read-string "Move to: ")))
    (causal-ucm--send (format "move %s %s" old new))))

(defun causal-ucm-alias-term ()
  "UCM: create a term alias."
  (interactive)
  (let ((existing (read-string "Existing name: "))
        (new (read-string "Alias name: ")))
    (causal-ucm--send (format "alias.term %s %s" existing new))))

(defun causal-ucm-compile ()
  "UCM: compile a Unison program to native binary."
  (interactive)
  (let ((main (read-string "Main function: "))
        (out (read-string "Output file: ")))
    (causal-ucm--send (format "compile %s %s" main out))))

(defun causal-ucm-upgrade ()
  "UCM: upgrade a library dependency."
  (interactive)
  (let ((old (read-string "Old dependency: "))
        (new (read-string "New dependency: ")))
    (causal-ucm--send (format "upgrade %s %s" old new))))

(provide 'causal-ucm-utils)
;;; causal-ucm-utils.el ends here
