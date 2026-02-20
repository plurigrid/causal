;;; causal-proof-settings.el --- Causal Proof Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Settings, customization, and about for causal-proof.
;;
;; For Amp: this file provides the settings Transient submenu, backend
;; listing display, and about info. The main entry points are:
;;   `causal-proof-settings-tmenu' — settings menu (from main menu via ",")
;;   `causal-proof-list-backends'  — display all registered backends + ops
;;   `causal-proof-about'          — describe the unified porcelain

;;; Code:
(require 'transient)
(require 'causal-lib)
(require 'causal-proof-utils)

;;; Customization

(defgroup causal-proof nil
  "Causal Proof settings."
  :group 'tools
  :prefix "causal-proof-")

(defcustom causal-proof-window-policy 'hybrid
  "Window layout policy for proof assistant buffers."
  :type '(choice (const :tag "Hybrid" hybrid)
                 (const :tag "Three windows" three)
                 (const :tag "Two windows" two))
  :group 'causal-proof)

;;; Settings Menu

(transient-define-prefix causal-proof-settings-tmenu ()
  "Causal Proof settings."
  ["Customize"
   [("w" "Window layout policy"
     (lambda () (interactive) (customize-variable 'causal-proof-window-policy)))
    ("G" "Proof Group"
     (lambda () (interactive) (customize-group "causal-proof")))
    ("B" "List backends"
     causal-proof-list-backends)]]

  [:class transient-row
   (causal-lib-customize-unicode)
   (causal-lib-customize-hide-navigation)]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-proof-about :transient nil)
   (causal-lib-quit-all)])

;;; Backend listing

(defun causal-proof-list-backends ()
  "Display registered proof backends."
  (interactive)
  (with-current-buffer (get-buffer-create "*Causal Proof Backends*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Causal Proof — Registered Backends\n")
      (insert (make-string 50 ?─) "\n\n")
      (dolist (b causal-proof-backends)
        (let* ((name (plist-get b :name))
               (active (ignore-errors (funcall (plist-get b :detect))))
               (ops (cl-loop for (k v) on b by #'cddr
                             unless (memq k '(:name :detect))
                             when v collect k)))
          (insert (format "  %s %s\n" (if active "*" " ") name))
          (insert (format "    ops: %s\n\n"
                          (mapconcat (lambda (k) (substring (symbol-name k) 1))
                                     ops ", ")))))
      (special-mode))
    (display-buffer (current-buffer))))

;;; About

(defun causal-proof-about-info ()
  "Causal Proof is a unified Transient porcelain for proof assistants.

Supported backends (extensible):
  Proof General (Coq, Narya, Isabelle)
  Lean 4
  Narya (standalone)
  OCaml / OxCaml (via Merlin / ocaml-eglot)
  DoubleTT (CatColab)

Register new backends with `causal-proof-register-backend'.

Source: URL `https://github.com/plurigrid/causal'"
  (ignore))

(defun causal-proof-about ()
  "About Causal Proof."
  (interactive)
  (describe-function #'causal-proof-about-info))

(provide 'causal-proof-settings)
;;; causal-proof-settings.el ends here
