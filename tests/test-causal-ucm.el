;;; test-causal-ucm.el --- ERT tests for causal-ucm  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Offline ERT tests for causal-ucm, causal-ucm-utils, and
;; causal-ucm-settings.  No UCM process needed; all process
;; interaction is stubbed with cl-letf.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'causal-ucm-utils)
(require 'causal-ucm-settings)

;;; ── Settings Defaults ────────────────────────────────────────────────

(ert-deftest test-causal-ucm-executable-default ()
  "Default UCM executable is \"ucm\"."
  (should (string= "ucm" (default-value 'causal-ucm-executable))))

(ert-deftest test-causal-ucm-customization-group ()
  "causal-ucm customization group exists."
  (should (get 'causal-ucm 'custom-group)))

;;; ── Unicode Database ─────────────────────────────────────────────────

(ert-deftest test-causal-ucm-unicode-db-keys ()
  "All expected keys resolve in the unicode DB."
  (let ((keys '(:add :update :delete :find :view :edit :branch
                :merge :switch :push :pull :test :run :project
                :history :undo :ls :todo :docs :clone :lib)))
    (dolist (key keys)
      (should (stringp (causal-ucm-unicode-get key))))))

(ert-deftest test-causal-ucm-unicode-db-complete ()
  "Unicode DB has exactly 21 entries."
  (should (= 21 (length causal-ucm-unicode-db))))

;;; ── Process Management ───────────────────────────────────────────────

(ert-deftest test-causal-ucm-live-p-no-buffer ()
  "live-p returns nil when no UCM buffer exists."
  (when (get-buffer causal-ucm--process-buffer)
    (kill-buffer causal-ucm--process-buffer))
  (should (null (causal-ucm--live-p))))

(ert-deftest test-causal-ucm-process-buffer-name ()
  "Process buffer is named *ucm*."
  (should (string= "*ucm*" causal-ucm--process-buffer)))

;;; ── Command Dispatch ─────────────────────────────────────────────────

(ert-deftest test-causal-ucm-send-dispatches ()
  "send dispatches command string to the process buffer."
  (let ((sent-cmd nil))
    (cl-letf (((symbol-function 'causal-ucm--live-p) (lambda () t))
              ((symbol-function 'comint-send-input) (lambda () nil))
              ((symbol-function 'insert) (lambda (s) (setq sent-cmd s))))
      (with-temp-buffer
        (rename-buffer causal-ucm--process-buffer t)
        (causal-ucm--send "test.all")
        (should (string= "test.all" sent-cmd))))))

(ert-deftest test-causal-ucm-send-with-arg-formats ()
  "send-with-arg formats command + argument correctly."
  (let ((sent-cmd nil))
    (cl-letf (((symbol-function 'causal-ucm--live-p) (lambda () t))
              ((symbol-function 'comint-send-input) (lambda () nil))
              ((symbol-function 'insert) (lambda (s) (setq sent-cmd s)))
              ((symbol-function 'read-string) (lambda (_p) "myTerm")))
      (with-temp-buffer
        (rename-buffer causal-ucm--process-buffer t)
        (causal-ucm--send-with-arg "view" "View: ")
        (should (string= "view myTerm" sent-cmd))))))

(ert-deftest test-causal-ucm-send-with-arg-empty ()
  "send-with-arg sends bare command when arg is empty."
  (let ((sent-cmd nil))
    (cl-letf (((symbol-function 'causal-ucm--live-p) (lambda () t))
              ((symbol-function 'comint-send-input) (lambda () nil))
              ((symbol-function 'insert) (lambda (s) (setq sent-cmd s)))
              ((symbol-function 'read-string) (lambda (_p) "")))
      (with-temp-buffer
        (rename-buffer causal-ucm--process-buffer t)
        (causal-ucm--send-with-arg "add" "Add: ")
        (should (string= "add" sent-cmd))))))

;;; ── About ────────────────────────────────────────────────────────────

(ert-deftest test-causal-ucm-about-function-exists ()
  "About function is defined."
  (should (fboundp 'causal-ucm-about)))

(ert-deftest test-causal-ucm-about-ucm-docstring ()
  "About docstring mentions Unison."
  (should (string-match-p "Unison"
                          (documentation 'causal-ucm-about-ucm))))

(provide 'test-causal-ucm)
;;; test-causal-ucm.el ends here
