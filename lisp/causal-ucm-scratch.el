;;; causal-ucm-scratch.el --- JIT moldable scratch pads for UCM  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Just-in-time moldable scratch pad interfaces for optimal control
;; surfaces in the Unison Codebase Manager.
;;
;; UCM's workflow: edit .u scratch files -> add/update into codebase.
;; This module creates context-shaped buffers on demand:
;;
;;   - TERM scratch:   edit a single definition with its type sig
;;   - EXPLORE scratch: dependents/dependencies view with inline edit
;;   - TEST scratch:    write and run tests for a term
;;   - NAMESPACE scratch: bulk-edit an entire namespace
;;   - MERGE scratch:  conflict resolution workspace
;;
;; Each scratch pad carries metadata (origin term, namespace, intent)
;; and exposes a Transient control surface tuned to its context.
;; Buffers are ephemeral by default; `add` or `update` flushes them
;; into the codebase and kills the pad.

;;; Code:

(require 'causal-ucm-utils)
(require 'causal-lib)

;;; --- Scratch pad types ---

(defvar-local causal-ucm-scratch--type nil
  "Type of this scratch pad: term, explore, test, namespace, merge.")

(defvar-local causal-ucm-scratch--origin nil
  "The term or namespace this scratch pad was created for.")

(defvar-local causal-ucm-scratch--intent nil
  "Intent: add, update, test, explore, merge.")

(defvar-local causal-ucm-scratch--namespace nil
  "Namespace context for this scratch pad.")

;;; --- Minor mode ---

(defvar causal-ucm-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-c" #'causal-ucm-scratch-commit)
    (keymap-set map "C-c C-k" #'causal-ucm-scratch-abort)
    (keymap-set map "C-c C-t" #'causal-ucm-scratch-test)
    (keymap-set map "C-c C-v" #'causal-ucm-scratch-view-origin)
    (keymap-set map "C-c C-d" #'causal-ucm-scratch-deps)
    (keymap-set map "C-o" #'causal-ucm-scratch-tmenu)
    map))

(define-minor-mode causal-ucm-scratch-mode
  "Minor mode for UCM scratch pad buffers.
Provides contextual keybindings and Transient control surfaces."
  :lighter " UCM-scratch"
  :keymap causal-ucm-scratch-mode-map
  (when causal-ucm-scratch-mode
    (setq-local header-line-format
                (causal-ucm-scratch--header-line))))

;;; --- Buffer creation (JIT) ---

(defun causal-ucm-scratch--make-buffer-name (type origin)
  "Generate buffer name for scratch TYPE and ORIGIN term/namespace."
  (format "*ucm-%s:%s*" type (or origin "new")))

(defun causal-ucm-scratch--create (type &optional origin intent namespace template)
  "Create a JIT scratch pad of TYPE for ORIGIN.
INTENT is one of: add, update, test, explore, merge.
NAMESPACE is the UCM namespace context.
TEMPLATE is initial buffer content, or nil for empty."
  (let* ((name (causal-ucm-scratch--make-buffer-name type origin))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (erase-buffer)
      (when template (insert template))
      (goto-char (point-min))
      (setq causal-ucm-scratch--type type)
      (setq causal-ucm-scratch--origin origin)
      (setq causal-ucm-scratch--intent (or intent 'add))
      (setq causal-ucm-scratch--namespace namespace)
      (causal-ucm-scratch-mode 1)
      (set-buffer-modified-p nil))
    (pop-to-buffer buf)
    buf))

(defun causal-ucm-scratch--header-line ()
  "Build header-line showing scratch pad context."
  (format " UCM %s | %s | intent: %s | ns: %s"
          (or causal-ucm-scratch--type "?")
          (or causal-ucm-scratch--origin "new")
          (or causal-ucm-scratch--intent "?")
          (or causal-ucm-scratch--namespace ".")))

;;; --- JIT constructors per scratch type ---

(defun causal-ucm-scratch-term (term)
  "Open a scratch pad to edit TERM."
  (interactive "sTerm to edit: ")
  (causal-ucm--send (format "edit %s" term))
  (causal-ucm-scratch--create
   'term term 'update nil
   (format "-- Editing: %s\n-- Type C-c C-c to update, C-c C-k to abort\n\n" term)))

(defun causal-ucm-scratch-new ()
  "Open a blank scratch pad for new definitions."
  (interactive)
  (causal-ucm-scratch--create
   'term nil 'add nil
   "-- New definition scratch pad\n-- Type C-c C-c to add, C-c C-k to abort\n\n"))

(defun causal-ucm-scratch-test (term)
  "Open a test scratch pad for TERM."
  (interactive
   (list (or causal-ucm-scratch--origin
             (read-string "Test term: "))))
  (causal-ucm-scratch--create
   'test term 'test nil
   (format (concat "-- Test scratch for: %s\n"
                   "-- C-c C-c to run io.test, C-c C-k to abort\n\n"
                   "%s.tests.%s = Tests.check \"%s works\" do\n"
                   "  use Nat\n"
                   "  checkEqual (%s 1) expected\n")
           term
           (or causal-ucm-scratch--namespace "")
           term term term)))

(defun causal-ucm-scratch-namespace (ns)
  "Open a scratch pad for editing namespace NS."
  (interactive "sNamespace: ")
  (causal-ucm--send (format "edit.namespace %s" ns))
  (causal-ucm-scratch--create
   'namespace ns 'update ns
   (format "-- Namespace: %s\n-- Bulk edit. C-c C-c to update all.\n\n" ns)))

(defun causal-ucm-scratch-explore (term)
  "Open an exploration scratch pad for TERM.
Shows dependencies and dependents inline."
  (interactive "sTerm to explore: ")
  (causal-ucm--send (format "view %s" term))
  (causal-ucm--send (format "dependencies %s" term))
  (causal-ucm-scratch--create
   'explore term 'explore nil
   (format (concat "-- Explore: %s\n"
                   "-- Dependencies and dependents shown in *ucm* buffer\n"
                   "-- Edit below, C-c C-c to update\n\n")
           term)))

(defun causal-ucm-scratch-merge (branch)
  "Open a merge conflict resolution scratch pad for BRANCH."
  (interactive "sBranch to merge: ")
  (causal-ucm-scratch--create
   'merge branch 'merge nil
   (format (concat "-- Merge resolution: %s -> current\n"
                   "-- Resolve conflicts below, C-c C-c to merge.commit\n\n")
           branch)))

;;; --- Scratch pad actions (control surface) ---

(defun causal-ucm-scratch-commit ()
  "Commit this scratch pad into the codebase.
Dispatches add, update, test, or merge based on intent."
  (interactive)
  (let ((intent causal-ucm-scratch--intent)
        (origin causal-ucm-scratch--origin)
        (content (buffer-string)))
    (pcase intent
      ('add
       (causal-ucm-scratch--write-and-load content)
       (causal-ucm--send (if origin (format "add %s" origin) "add")))
      ('update
       (causal-ucm-scratch--write-and-load content)
       (causal-ucm--send (if origin (format "update %s" origin) "update")))
      ('test
       (causal-ucm-scratch--write-and-load content)
       (causal-ucm--send "add")
       (causal-ucm--send (if origin (format "io.test %s" origin) "test")))
      ('merge
       (causal-ucm--send "merge.commit"))
      ('explore
       (causal-ucm-scratch--write-and-load content)
       (causal-ucm--send "update"))
      (_
       (causal-ucm-scratch--write-and-load content)
       (causal-ucm--send "add")))
    (message "UCM scratch [%s]: committed %s" intent (or origin "all"))
    (causal-ucm-scratch--maybe-kill)))

(defun causal-ucm-scratch-abort ()
  "Abort this scratch pad without committing."
  (interactive)
  (when (or (not (buffer-modified-p))
            (yes-or-no-p "Scratch pad modified. Abort anyway? "))
    (message "UCM scratch aborted.")
    (causal-ucm-scratch--maybe-kill)))

(defun causal-ucm-scratch-view-origin ()
  "View the origin term in UCM."
  (interactive)
  (if causal-ucm-scratch--origin
      (causal-ucm--send (format "view %s" causal-ucm-scratch--origin))
    (message "No origin term set.")))

(defun causal-ucm-scratch-deps ()
  "Show dependencies of the origin term."
  (interactive)
  (if causal-ucm-scratch--origin
      (progn
        (causal-ucm--send (format "dependencies %s" causal-ucm-scratch--origin))
        (causal-ucm--send (format "dependents %s" causal-ucm-scratch--origin)))
    (message "No origin term set.")))

;;; --- Internal helpers ---

(defun causal-ucm-scratch--write-and-load (content)
  "Write CONTENT to a temporary .u file and load it into UCM."
  (let ((tmpfile (expand-file-name
                  (format "causal-scratch-%s.u"
                          (format-time-string "%s"))
                  temporary-file-directory)))
    (with-temp-file tmpfile
      (insert content))
    (causal-ucm--send (format "load %s" tmpfile))))

(defun causal-ucm-scratch--maybe-kill ()
  "Kill the current scratch pad buffer."
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer)))

(defun causal-ucm-scratch--active-p ()
  "Return non-nil if current buffer is a UCM scratch pad."
  (bound-and-true-p causal-ucm-scratch-mode))

(defun causal-ucm-scratch--type-label ()
  "Return display label for current scratch type."
  (pcase causal-ucm-scratch--type
    ('term "Term")
    ('test "Test")
    ('namespace "Namespace")
    ('explore "Explore")
    ('merge "Merge")
    (_ "Scratch")))

;;; --- Transient control surface (context-sensitive) ---

;;;###autoload (autoload 'causal-ucm-scratch-tmenu "causal-ucm-scratch" nil t)
(transient-define-prefix causal-ucm-scratch-tmenu ()
  "Control surface for the current UCM scratch pad."
  :refresh-suffixes t
  [:description
   (lambda ()
     (format "UCM Scratch: %s [%s] intent=%s"
             (or causal-ucm-scratch--origin "new")
             (causal-ucm-scratch--type-label)
             (or causal-ucm-scratch--intent "?")))
   ["Actions"
    :pad-keys t
    ("c" "Commit (add/update)" causal-ucm-scratch-commit)
    ("k" "Abort" causal-ucm-scratch-abort)
    ("t" "Run test" causal-ucm-scratch-run-test
     :if (lambda () (eq causal-ucm-scratch--intent 'test)))
    ("m" "Merge commit" causal-ucm-scratch-merge-commit
     :if (lambda () (eq causal-ucm-scratch--intent 'merge)))]

   ["Inspect"
    :pad-keys t
    ("v" "View origin" causal-ucm-scratch-view-origin)
    ("d" "Deps / Dependents" causal-ucm-scratch-deps)
    ("D" "Diff preview" causal-ucm-scratch-diff-preview)]

   ["Navigate"
    :pad-keys t
    ("n" "New scratch" causal-ucm-scratch-new)
    ("e" "Edit term…" causal-ucm-scratch-term)
    ("N" "Edit namespace…" causal-ucm-scratch-namespace)
    ("x" "Explore term…" causal-ucm-scratch-explore)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("," "UCM menu" causal-ucm-tmenu)
   (causal-lib-quit-all)])

;;; --- Extra scratch actions ---

(defun causal-ucm-scratch-run-test ()
  "Run the test defined in this scratch pad."
  (interactive)
  (causal-ucm-scratch--write-and-load (buffer-string))
  (causal-ucm--send "add")
  (if causal-ucm-scratch--origin
      (causal-ucm--send (format "io.test %s" causal-ucm-scratch--origin))
    (causal-ucm--send "test")))

(defun causal-ucm-scratch-merge-commit ()
  "Commit the merge from this scratch pad."
  (interactive)
  (causal-ucm--send "merge.commit")
  (causal-ucm-scratch--maybe-kill))

(defun causal-ucm-scratch-diff-preview ()
  "Preview what update would change."
  (interactive)
  (causal-ucm-scratch--write-and-load (buffer-string))
  (causal-ucm--send "diff.update"))

(provide 'causal-ucm-scratch)
;;; causal-ucm-scratch.el ends here
