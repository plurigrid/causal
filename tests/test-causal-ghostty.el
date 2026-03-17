;;; test-causal-ghostty.el --- ERT tests for causal-ghostty -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, terminal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Offline ERT tests for causal-ghostty, causal-ghostty-utils, and
;; causal-ghostty-settings.  No ghostty-emacs runtime needed; all
;; ghostty functions are stubbed with cl-letf.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'causal-ghostty-test-utils)
(require 'causal-ghostty-utils)
(require 'causal-ghostty-settings)

;;; ── Settings Defaults ────────────────────────────────────────────────

(ert-deftest test-causal-ghostty-default-quantization ()
  "Default color quantization is rgb24."
  (should (string= "rgb24" (default-value 'causal-ghostty-quantization))))

(ert-deftest test-causal-ghostty-libghostty-path-default ()
  "Default libghostty path points to ~/i/libghostty-vt."
  (should (string-match-p "libghostty-vt"
                          (default-value 'causal-ghostty-libghostty-path))))

(ert-deftest test-causal-ghostty-module-path-default ()
  "Default module path points to ~/i/ghostty-emacs."
  (should (string-match-p "ghostty-emacs"
                          (default-value 'causal-ghostty-module-path))))

(ert-deftest test-causal-ghostty-customization-group ()
  "causal-ghostty customization group exists."
  (should (get 'causal-ghostty 'custom-group)))

;;; ── Unicode Database ─────────────────────────────────────────────────

(ert-deftest test-causal-ghostty-unicode-db-keys ()
  "All expected keys resolve in the unicode DB."
  (let ((keys '(:scroll-up :scroll-down :refresh :terminal :split-h :split-v)))
    (dolist (key keys)
      (should (stringp (causal-ghostty-unicode-get key))))))

(ert-deftest test-causal-ghostty-unicode-db-complete ()
  "Unicode DB has exactly the expected number of entries."
  (should (= 6 (length causal-ghostty-unicode-db))))

;;; ── Terminal Active Predicate ────────────────────────────────────────

(ert-deftest test-causal-ghostty-terminal-active-p-no-ghostty ()
  "terminal-active-p returns nil when ghostty is not loaded."
  (cl-letf (((symbol-function 'fboundp) (lambda (s) nil)))
    (should (null (causal-ghostty--terminal-active-p)))))

(ert-deftest test-causal-ghostty-terminal-active-p-with-terminal ()
  "terminal-active-p returns non-nil when ghostty reports active terminal."
  (cl-letf (((symbol-function 'ghostty--get-terminal-for-buffer)
             (lambda () 'mock-terminal)))
    (should (causal-ghostty--terminal-active-p))))

(ert-deftest test-causal-ghostty-terminal-active-p-no-terminal ()
  "terminal-active-p returns nil when ghostty reports no terminal."
  (cl-letf (((symbol-function 'ghostty--get-terminal-for-buffer)
             (lambda () nil)))
    (should (null (causal-ghostty--terminal-active-p)))))

;;; ── Force Render ─────────────────────────────────────────────────────

(ert-deftest test-causal-ghostty-force-render-delegates ()
  "force-render calls ghostty--render-frame when available."
  (let ((called nil))
    (cl-letf (((symbol-function 'ghostty--render-frame)
               (lambda () (setq called t))))
      (causal-ghostty-force-render)
      (should called))))

(ert-deftest test-causal-ghostty-force-render-fallback ()
  "force-render messages when ghostty not loaded."
  (let ((msg nil))
    (cl-letf (((symbol-function 'fboundp) (lambda (s) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (causal-ghostty-force-render)
      (should (string-match-p "not loaded" msg)))))

;;; ── Cycle Quantization ───────────────────────────────────────────────

(ert-deftest test-causal-ghostty-cycle-quantization-rgb24-to-xterm256 ()
  "Cycling from rgb24 advances to xterm256."
  (let ((causal-ghostty-quantization "rgb24")
        (msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (causal-ghostty-cycle-quantization)
      (should (string= "xterm256" causal-ghostty-quantization)))))

(ert-deftest test-causal-ghostty-cycle-quantization-xterm256-to-xterm16 ()
  "Cycling from xterm256 advances to xterm16."
  (let ((causal-ghostty-quantization "xterm256"))
    (cl-letf (((symbol-function 'message) #'ignore))
      (causal-ghostty-cycle-quantization)
      (should (string= "xterm16" causal-ghostty-quantization)))))

(ert-deftest test-causal-ghostty-cycle-quantization-wraps ()
  "Cycling from auto wraps back to rgb24."
  (let ((causal-ghostty-quantization "auto"))
    (cl-letf (((symbol-function 'message) #'ignore))
      (causal-ghostty-cycle-quantization)
      (should (string= "rgb24" causal-ghostty-quantization)))))

(ert-deftest test-causal-ghostty-cycle-quantization-messages ()
  "Cycling quantization reports the new mode via message."
  (let ((causal-ghostty-quantization "rgb24")
        (msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (causal-ghostty-cycle-quantization)
      (should (string-match-p "xterm256" msg)))))

;;; ── Module Status ────────────────────────────────────────────────────

(ert-deftest test-causal-ghostty-show-module-status-not-loaded ()
  "show-module-status reports NOT loaded when module absent."
  (let ((msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (causal-ghostty-show-module-status)
      (should (string-match-p "NOT loaded" msg)))))

(ert-deftest test-causal-ghostty-show-module-status-reports-functions ()
  "show-module-status reports terminal-new and terminal-write status."
  (let ((msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (causal-ghostty-show-module-status)
      (should (string-match-p "terminal-new" msg))
      (should (string-match-p "terminal-write" msg)))))

;;; ── VT Version ───────────────────────────────────────────────────────

(ert-deftest test-causal-ghostty-show-vt-version-no-dir ()
  "show-vt-version messages 'not found' when path doesn't exist."
  (let ((causal-ghostty-libghostty-path "/nonexistent/path")
        (msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (causal-ghostty-show-vt-version)
      (should (string-match-p "not found" msg)))))

(ert-deftest test-causal-ghostty-show-vt-version-with-version-file ()
  "show-vt-version extracts version from build.zig.zon."
  (let* ((tmpdir (make-temp-file "ghostty-test-" t))
         (causal-ghostty-libghostty-path tmpdir)
         (zon-file (expand-file-name "build.zig.zon" tmpdir))
         (msg nil))
    (unwind-protect
        (progn
          (with-temp-file zon-file
            (insert ".{\n    .name = \"libghostty-vt\",\n    .version = \"0.7.2\",\n}\n"))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
            (causal-ghostty-show-vt-version)
            (should (string-match-p "0\\.7\\.2" msg))))
      (delete-directory tmpdir t))))

(ert-deftest test-causal-ghostty-show-vt-version-no-zon ()
  "show-vt-version messages missing build.zig.zon."
  (let* ((tmpdir (make-temp-file "ghostty-test-" t))
         (causal-ghostty-libghostty-path tmpdir)
         (msg nil))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
          (causal-ghostty-show-vt-version)
          (should (string-match-p "not found" msg)))
      (delete-directory tmpdir t))))

;;; ── About ────────────────────────────────────────────────────────────

(ert-deftest test-causal-ghostty-about-message ()
  "about produces a message mentioning ghostty-emacs."
  (let ((msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (causal-ghostty-about)
      (should (string-match-p "ghostty-emacs" msg)))))

(ert-deftest test-causal-ghostty-about-mentions-libghostty ()
  "about message mentions libghostty-vt."
  (let ((msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (causal-ghostty-about)
      (should (string-match-p "libghostty-vt" msg)))))

(provide 'test-causal-ghostty)
;;; test-causal-ghostty.el ends here
