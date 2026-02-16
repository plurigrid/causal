;;; causal-ghostty-utils.el --- Utilities for Causal Ghostty -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Plurigrid

;; Author: Plurigrid
;; Keywords: terminal, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Utility functions for the Causal Ghostty porcelain.

;;; Code:

(require 'causal-lib)

(defconst causal-ghostty-unicode-db
  '((:scroll-up . '("↑ Scroll Up" "Scroll Up"))
    (:scroll-down . '("↓ Scroll Down" "Scroll Down"))
    (:refresh . '("⟳ Refresh" "Refresh"))
    (:terminal . '("▣ Terminal" "Terminal"))
    (:split-h . '("━ Split" "Split Horiz"))
    (:split-v . '("┃ Split" "Split Vert")))
  "Unicode symbol DB for Causal Ghostty.")

(defun causal-ghostty-unicode-get (key)
  "Lookup Unicode symbol for KEY in Ghostty DB."
  (causal-lib-unicode-db-get key causal-ghostty-unicode-db))

(defun causal-ghostty--terminal-active-p ()
  "Return non-nil if a ghostty terminal is active in current buffer."
  (and (fboundp 'ghostty--get-terminal-for-buffer)
       (ghostty--get-terminal-for-buffer)))

(defun causal-ghostty-force-render ()
  "Force a full terminal re-render."
  (interactive)
  (if (fboundp 'ghostty--render-frame)
      (ghostty--render-frame)
    (message "ghostty-emacs not loaded")))

(defun causal-ghostty-cycle-quantization ()
  "Cycle through color quantization modes."
  (interactive)
  (let* ((modes '("rgb24" "xterm256" "xterm16" "auto"))
         (current (or (bound-and-true-p causal-ghostty-quantization) "rgb24"))
         (idx (cl-position current modes :test #'string=))
         (next (nth (mod (1+ (or idx 0)) (length modes)) modes)))
    (setq causal-ghostty-quantization next)
    (message "Color quantization: %s" next)))

(defun causal-ghostty-show-module-status ()
  "Show ghostty-emacs module loading status."
  (interactive)
  (let ((loaded (bound-and-true-p ghostty--module-loaded))
        (has-new (fboundp 'ghostty-terminal-new))
        (has-write (fboundp 'ghostty-terminal-write)))
    (message "ghostty module: %s | terminal-new: %s | terminal-write: %s"
             (if loaded "loaded" "NOT loaded")
             (if has-new "yes" "no")
             (if has-write "yes" "no"))))

(defun causal-ghostty-show-vt-version ()
  "Show libghostty-vt version info."
  (interactive)
  (let ((lib-path (or (bound-and-true-p causal-ghostty-libghostty-path)
                      "~/i/libghostty-vt")))
    (if (file-directory-p lib-path)
        (let ((version-file (expand-file-name "build.zig.zon" lib-path)))
          (if (file-exists-p version-file)
              (with-temp-buffer
                (insert-file-contents version-file)
                (if (re-search-forward "\\.version\\s*=\\s*\"\\([^\"]+\\)\"" nil t)
                    (message "libghostty-vt version: %s (at %s)" (match-string 1) lib-path)
                  (message "libghostty-vt at %s (version not found in build.zig.zon)" lib-path)))
            (message "libghostty-vt: build.zig.zon not found at %s" lib-path)))
      (message "libghostty-vt not found at %s" lib-path))))

(defun causal-ghostty-about ()
  "Show about information for Causal Ghostty."
  (interactive)
  (message "Causal Ghostty: Transient porcelain for ghostty-emacs (libghostty-vt)
Part of plurigrid/causal — fork of kickingvegas/casual
Terminal engine: libghostty-vt by Mitchell Hashimoto
Rendering: Canvas 2D (two-pass) | Trapezoid GPU (future)"))

(defun causal-ghostty-info-libghostty ()
  "Open libghostty-vt info in browser."
  (interactive)
  (browse-url "https://mitchellh.com/writing/libghostty-is-coming"))

(provide 'causal-ghostty-utils)
;;; causal-ghostty-utils.el ends here
