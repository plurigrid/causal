;;; causal-ghostty-settings.el --- Settings for Causal Ghostty -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Plurigrid

;; Author: Plurigrid
;; Keywords: terminal, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Settings menu for Causal Ghostty.

;;; Code:

(require 'transient)

(defgroup causal-ghostty nil
  "Settings for Causal Ghostty terminal interface."
  :group 'causal
  :prefix "causal-ghostty-")

(defcustom causal-ghostty-quantization "rgb24"
  "Color quantization mode for terminal rendering.
Possible values: \"auto\", \"rgb24\", \"xterm256\", \"xterm16\"."
  :type '(choice (const "auto")
                 (const "rgb24")
                 (const "xterm256")
                 (const "xterm16"))
  :group 'causal-ghostty)

(defcustom causal-ghostty-libghostty-path
  (expand-file-name "~/i/libghostty-vt")
  "Path to libghostty-vt source tree."
  :type 'directory
  :group 'causal-ghostty)

(defcustom causal-ghostty-module-path
  (expand-file-name "~/i/ghostty-emacs")
  "Path to ghostty-emacs module directory."
  :type 'directory
  :group 'causal-ghostty)

;;;###autoload (autoload 'causal-ghostty-settings-tmenu "causal-ghostty-settings" nil t)
(transient-define-prefix causal-ghostty-settings-tmenu ()
  "Settings menu for Causal Ghostty."
  ["Causal Ghostty Settings"
   ["Rendering"
    ("q" "Color Quantization"
     (lambda () (interactive)
       (customize-variable 'causal-ghostty-quantization)))
    ("r" "Rendering Mode"
     (lambda () (interactive)
       (when (boundp 'ghostty-rendering-mode)
         (customize-variable 'ghostty-rendering-mode))))]

   ["Terminal"
    ("c" "Default Columns"
     (lambda () (interactive)
       (when (boundp 'ghostty-default-cols)
         (customize-variable 'ghostty-default-cols))))
    ("w" "Default Rows"
     (lambda () (interactive)
       (when (boundp 'ghostty-default-rows)
         (customize-variable 'ghostty-default-rows))))
    ("p" "Palette"
     (lambda () (interactive)
       (when (boundp 'ghostty-palette-name)
         (customize-variable 'ghostty-palette-name))))]

   ["Paths"
    ("l" "libghostty-vt Path"
     (lambda () (interactive)
       (customize-variable 'causal-ghostty-libghostty-path)))
    ("m" "Module Path"
     (lambda () (interactive)
       (customize-variable 'causal-ghostty-module-path)))]]

  [:class transient-row
   (causal-lib-quit-one)
   (causal-lib-quit-all)])

(provide 'causal-ghostty-settings)
;;; causal-ghostty-settings.el ends here
