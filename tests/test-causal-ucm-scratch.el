;;; test-causal-ucm-scratch.el --- ERT tests for causal-ucm-scratch  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT tests for the JIT moldable scratch pad system.  No UCM process
;; needed; all process interaction is stubbed.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'causal-ucm-scratch)

;;; ── Buffer Creation ──────────────────────────────────────────────────

(ert-deftest test-ucm-scratch-buffer-name-term ()
  "Buffer name includes type and origin."
  (should (string= "*ucm-term:foo*"
                    (causal-ucm-scratch--make-buffer-name 'term "foo"))))

(ert-deftest test-ucm-scratch-buffer-name-nil-origin ()
  "Buffer name uses 'new' when origin is nil."
  (should (string= "*ucm-term:new*"
                    (causal-ucm-scratch--make-buffer-name 'term nil))))

(ert-deftest test-ucm-scratch-buffer-name-namespace ()
  "Buffer name for namespace type."
  (should (string= "*ucm-namespace:lib.base*"
                    (causal-ucm-scratch--make-buffer-name 'namespace "lib.base"))))

(ert-deftest test-ucm-scratch-create-sets-locals ()
  "Scratch creation sets buffer-local metadata."
  (cl-letf (((symbol-function 'causal-ucm--send) #'ignore)
            ((symbol-function 'pop-to-buffer) #'ignore))
    (let ((buf (causal-ucm-scratch--create 'term "myFn" 'update "myNs" "-- hello\n")))
      (unwind-protect
          (with-current-buffer buf
            (should (eq causal-ucm-scratch--type 'term))
            (should (string= causal-ucm-scratch--origin "myFn"))
            (should (eq causal-ucm-scratch--intent 'update))
            (should (string= causal-ucm-scratch--namespace "myNs"))
            (should causal-ucm-scratch-mode)
            (should (string-match-p "hello" (buffer-string))))
        (kill-buffer buf)))))

(ert-deftest test-ucm-scratch-create-empty ()
  "Scratch creation with no template yields empty content after metadata."
  (cl-letf (((symbol-function 'causal-ucm--send) #'ignore)
            ((symbol-function 'pop-to-buffer) #'ignore))
    (let ((buf (causal-ucm-scratch--create 'test "bar" 'test nil nil)))
      (unwind-protect
          (with-current-buffer buf
            (should (eq causal-ucm-scratch--type 'test))
            (should (string= (buffer-string) "")))
        (kill-buffer buf)))))

;;; ── Header Line ──────────────────────────────────────────────────────

(ert-deftest test-ucm-scratch-header-line ()
  "Header line includes all metadata fields."
  (cl-letf (((symbol-function 'causal-ucm--send) #'ignore)
            ((symbol-function 'pop-to-buffer) #'ignore))
    (let ((buf (causal-ucm-scratch--create 'explore "Map.map" 'explore "base" nil)))
      (unwind-protect
          (with-current-buffer buf
            (let ((hdr (causal-ucm-scratch--header-line)))
              (should (string-match-p "explore" hdr))
              (should (string-match-p "Map\\.map" hdr))
              (should (string-match-p "base" hdr))))
        (kill-buffer buf)))))

;;; ── Active Predicate ─────────────────────────────────────────────────

(ert-deftest test-ucm-scratch-active-p-in-scratch ()
  "active-p returns non-nil inside a scratch buffer."
  (cl-letf (((symbol-function 'causal-ucm--send) #'ignore)
            ((symbol-function 'pop-to-buffer) #'ignore))
    (let ((buf (causal-ucm-scratch--create 'term "x" 'add nil nil)))
      (unwind-protect
          (with-current-buffer buf
            (should (causal-ucm-scratch--active-p)))
        (kill-buffer buf)))))

(ert-deftest test-ucm-scratch-active-p-outside ()
  "active-p returns nil in a regular buffer."
  (with-temp-buffer
    (should (null (causal-ucm-scratch--active-p)))))

;;; ── Type Labels ──────────────────────────────────────────────────────

(ert-deftest test-ucm-scratch-type-labels ()
  "All scratch types have proper display labels."
  (cl-letf (((symbol-function 'causal-ucm--send) #'ignore)
            ((symbol-function 'pop-to-buffer) #'ignore))
    (dolist (pair '((term . "Term")
                    (test . "Test")
                    (namespace . "Namespace")
                    (explore . "Explore")
                    (merge . "Merge")))
      (let ((buf (causal-ucm-scratch--create (car pair) "z" 'add nil nil)))
        (unwind-protect
            (with-current-buffer buf
              (should (string= (causal-ucm-scratch--type-label) (cdr pair))))
          (kill-buffer buf))))))

;;; ── Commit Dispatch ──────────────────────────────────────────────────

(ert-deftest test-ucm-scratch-commit-add-intent ()
  "Commit with add intent sends load then add."
  (let ((cmds nil))
    (cl-letf (((symbol-function 'causal-ucm--send)
               (lambda (c) (push c cmds)))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'causal-ucm-scratch--maybe-kill) #'ignore))
      (let ((buf (causal-ucm-scratch--create 'term "newFn" 'add nil "myFn = 42\n")))
        (unwind-protect
            (with-current-buffer buf
              (causal-ucm-scratch-commit)
              (should (cl-some (lambda (c) (string-match-p "^load " c)) cmds))
              (should (member "add newFn" cmds)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest test-ucm-scratch-commit-update-intent ()
  "Commit with update intent sends load then update."
  (let ((cmds nil))
    (cl-letf (((symbol-function 'causal-ucm--send)
               (lambda (c) (push c cmds)))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'causal-ucm-scratch--maybe-kill) #'ignore))
      (let ((buf (causal-ucm-scratch--create 'term "oldFn" 'update nil "oldFn = 99\n")))
        (unwind-protect
            (with-current-buffer buf
              (causal-ucm-scratch-commit)
              (should (cl-some (lambda (c) (string-match-p "^load " c)) cmds))
              (should (member "update oldFn" cmds)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest test-ucm-scratch-commit-test-intent ()
  "Commit with test intent sends load, add, then io.test."
  (let ((cmds nil))
    (cl-letf (((symbol-function 'causal-ucm--send)
               (lambda (c) (push c cmds)))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'causal-ucm-scratch--maybe-kill) #'ignore))
      (let ((buf (causal-ucm-scratch--create 'test "myTest" 'test nil "test code\n")))
        (unwind-protect
            (with-current-buffer buf
              (causal-ucm-scratch-commit)
              (should (member "add" cmds))
              (should (member "io.test myTest" cmds)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest test-ucm-scratch-commit-merge-intent ()
  "Commit with merge intent sends merge.commit."
  (let ((cmds nil))
    (cl-letf (((symbol-function 'causal-ucm--send)
               (lambda (c) (push c cmds)))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'causal-ucm-scratch--maybe-kill) #'ignore))
      (let ((buf (causal-ucm-scratch--create 'merge "feature" 'merge nil "")))
        (unwind-protect
            (with-current-buffer buf
              (causal-ucm-scratch-commit)
              (should (member "merge.commit" cmds)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;; ── Abort ────────────────────────────────────────────────────────────

(ert-deftest test-ucm-scratch-abort-unmodified ()
  "Abort kills an unmodified scratch buffer."
  (cl-letf (((symbol-function 'causal-ucm--send) #'ignore)
            ((symbol-function 'pop-to-buffer) #'ignore))
    (let ((buf (causal-ucm-scratch--create 'term "x" 'add nil nil)))
      (with-current-buffer buf
        (set-buffer-modified-p nil)
        (causal-ucm-scratch-abort))
      (should (null (buffer-live-p buf))))))

;;; ── Write and Load ───────────────────────────────────────────────────

(ert-deftest test-ucm-scratch-write-and-load ()
  "write-and-load creates a .u file and sends load command."
  (let ((sent nil))
    (cl-letf (((symbol-function 'causal-ucm--send)
               (lambda (c) (setq sent c))))
      (causal-ucm-scratch--write-and-load "myFn = 1 + 1\n")
      (should (string-match-p "^load .*/causal-scratch-.*\\.u$" sent)))))

;;; ── Diff Preview ─────────────────────────────────────────────────────

(ert-deftest test-ucm-scratch-diff-preview-sends ()
  "Diff preview sends load + diff.update."
  (let ((cmds nil))
    (cl-letf (((symbol-function 'causal-ucm--send)
               (lambda (c) (push c cmds)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (let ((buf (causal-ucm-scratch--create 'term "z" 'update nil "z = 0\n")))
        (unwind-protect
            (with-current-buffer buf
              (causal-ucm-scratch-diff-preview)
              (should (member "diff.update" cmds))
              (should (cl-some (lambda (c) (string-match-p "^load " c)) cmds)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;; ── Keymap ───────────────────────────────────────────────────────────

(ert-deftest test-ucm-scratch-keymap-bindings ()
  "Key bindings are set in scratch mode map."
  (let ((map causal-ucm-scratch-mode-map))
    (should (eq (keymap-lookup map "C-c C-c") #'causal-ucm-scratch-commit))
    (should (eq (keymap-lookup map "C-c C-k") #'causal-ucm-scratch-abort))
    (should (eq (keymap-lookup map "C-c C-v") #'causal-ucm-scratch-view-origin))
    (should (eq (keymap-lookup map "C-c C-d") #'causal-ucm-scratch-deps))
    (should (eq (keymap-lookup map "C-o") #'causal-ucm-scratch-tmenu))))

(provide 'test-causal-ucm-scratch)
;;; test-causal-ucm-scratch.el ends here
