;;; causal-catcolab.el --- Transient UI for CatColab -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, categories
;; URL: https://github.com/plurigrid/causal

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

;; Transient-based Emacs porcelain for CatColab (catcolab.org) — the
;; Topos Institute's collaborative categorical modeling environment.
;;
;; QUICK START
;; -----------
;; (require 'causal-catcolab)
;; (keymap-set global-map "C-c C" #'causal-catcolab-tmenu)
;;
;; Works against backend.catcolab.org by default (no auth needed for
;; public documents).  For a local instance, set:
;;   (setq causal-catcolab-url "http://localhost:8000")
;;
;; INTEGRATION WITH causal-proofreader
;; ------------------------------------
;; M-x causal-proofreader-tmenu → proof nav
;; M-x causal-catcolab-save-proof-as-olog → snapshot proof state → CatColab olog

;;; Code:

(require 'transient)
(require 'causal-lib)
(require 'causal-catcolab-utils)

;;; Buffer Display Helpers

(defun causal-catcolab--display-stubs (stubs title &optional action-fn)
  "Display a list of RefStub STUBS in a buffer with TITLE.
If ACTION-FN is provided, RET on a line calls it with the ref-id."
  (with-current-buffer (get-buffer-create (format "*CatColab: %s*" title))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "CatColab — %s\n" title))
      (insert (format "Backend: %s\n" causal-catcolab-url))
      (insert (make-string 60 ?─) "\n\n")
      (if (or (null stubs) (zerop (length stubs)))
          (insert "  (no results)\n")
        (let ((items (alist-get 'items stubs))
              (total (alist-get 'total stubs 0)))
          (insert (format "  Showing %d of %d total\n\n" (length items) total))
          (mapc (lambda (stub)
                  (let ((line (causal-catcolab--stub-line stub))
                        (ref-id (alist-get 'refId stub "")))
                    (insert line)
                    (when action-fn
                      (make-text-button
                       (line-beginning-position) (line-end-position)
                       'action (lambda (_) (funcall action-fn ref-id))
                       'follow-link t
                       'help-echo (format "Open %s" ref-id)))
                    (insert "\n")))
                items))))
    (view-mode 1)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun causal-catcolab--display-snapshot (ref-id snapshot)
  "Display a JSON SNAPSHOT for REF-ID in a pretty-printed buffer."
  (with-current-buffer (get-buffer-create (format "*CatColab: %s*" ref-id))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "CatColab Snapshot: %s\n" ref-id))
      (insert (format "URL: %s\n" (causal-catcolab-url-for-ref ref-id)))
      (insert (make-string 60 ?─) "\n\n")
      (let ((name (if (vectorp snapshot) ""
                    (alist-get 'name snapshot "")))
            (type (if (vectorp snapshot) ""
                    (alist-get 'type snapshot ""))))
        (unless (string-empty-p name)
          (insert (format "Name:  %s\n" name)))
        (unless (string-empty-p type)
          (insert (format "Type:  %s\n\n" type))))
      (insert (pp-to-string snapshot)))
    (view-mode 1)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;; Commands

;;;###autoload
(defun causal-catcolab-browse (&optional query)
  "Browse public CatColab documents, optionally filtering by QUERY string."
  (interactive "sSearch (empty for all): ")
  (message "Fetching CatColab refs...")
  (let* ((q (if (string-empty-p query) nil query))
         (stubs (causal-catcolab-search-refs q 30)))
    (causal-catcolab--display-stubs
     stubs
     (if q (format "Search: %s" q) "Public Documents")
     #'causal-catcolab-view-ref)))

;;;###autoload
(defun causal-catcolab-view-ref (ref-id)
  "View the latest snapshot of CatColab ref REF-ID."
  (interactive "sRef UUID: ")
  (message "Fetching snapshot %s..." ref-id)
  (let ((snapshot (causal-catcolab-head-snapshot ref-id)))
    (causal-catcolab--display-snapshot ref-id snapshot)))

;;;###autoload
(defun causal-catcolab-open-in-browser (ref-id)
  "Open CatColab ref REF-ID in the system browser."
  (interactive "sRef UUID: ")
  (browse-url (causal-catcolab-url-for-ref ref-id)))

;;;###autoload
(defun causal-catcolab-new-olog (name)
  "Create a new simple-olog in CatColab with NAME."
  (interactive "sOlog name: ")
  (message "Creating olog '%s'..." name)
  (let* ((content (causal-catcolab--new-model "simple-olog" name))
         (ref-id (causal-catcolab-new-ref content)))
    (message "Created olog: %s" ref-id)
    (browse-url (causal-catcolab-url-for-ref ref-id))
    ref-id))

;;;###autoload
(defun causal-catcolab-new-causal-loop (name)
  "Create a new causal-loop diagram in CatColab with NAME."
  (interactive "sCausal loop name: ")
  (message "Creating causal loop '%s'..." name)
  (let* ((content (causal-catcolab--new-model "causal-loop" name))
         (ref-id (causal-catcolab-new-ref content)))
    (message "Created: %s" ref-id)
    (browse-url (causal-catcolab-url-for-ref ref-id))
    ref-id))

;;;###autoload
(defun causal-catcolab-new-petri-net (name)
  "Create a new Petri net in CatColab with NAME."
  (interactive "sPetri net name: ")
  (message "Creating Petri net '%s'..." name)
  (let* ((content (causal-catcolab--new-model "petri-net" name))
         (ref-id (causal-catcolab-new-ref content)))
    (message "Created: %s" ref-id)
    (browse-url (causal-catcolab-url-for-ref ref-id))
    ref-id))

;;;###autoload
(defun causal-catcolab-save-proof-as-olog ()
  "Snapshot the current proof context into a CatColab simple-olog.

Reads hypothesis names from the goals buffer or current buffer.
Creates an olog where:
  hypotheses → Types (objects)
  goal       → distinguished Type
  implications → Aspects (morphisms)"
  (interactive)
  (let* ((buf-name (buffer-name))
         (name (read-string "Olog name: "
                            (format "Proof: %s" buf-name)))
         ;; Scan current buffer for hypothesis-like lines
         ;; Works with Lean 4 and Proof General goal buffers
         (hyps (causal-catcolab--scan-hypotheses))
         (goal (causal-catcolab--scan-goal))
         (content (causal-catcolab--olog-from-proof name hyps goal))
         (ref-id (causal-catcolab-new-ref content)))
    (message "Proof olog created: %s" ref-id)
    (when (y-or-n-p "Open in browser? ")
      (browse-url (causal-catcolab-url-for-ref ref-id)))
    ref-id))

(defun causal-catcolab--scan-hypotheses ()
  "Extract hypothesis pairs from the current buffer or goals buffer.
Returns list of (name . type) pairs."
  ;; Try *Goals* buffer (Lean/PG), fall back to current buffer region
  (let ((goals-buf (get-buffer "*Goals*")))
    (if goals-buf
        (with-current-buffer goals-buf
          (causal-catcolab--parse-hypotheses (buffer-string)))
      (causal-catcolab--parse-hypotheses
       (if (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end))
         (buffer-string))))))

(defun causal-catcolab--parse-hypotheses (text)
  "Parse TEXT for hypothesis lines of the form `name : Type'.
Returns list of (name . type) pairs."
  (let ((hyps nil))
    (dolist (line (split-string text "\n"))
      (when (string-match "^\\s-*\\([[:alnum:]_']+\\)\\s-*:\\s-*\\([^⊢\n]+\\)" line)
        (let ((name (string-trim (match-string 1 line)))
              (type (string-trim (match-string 2 line))))
          (unless (member name '("case" "α" "β"))
            (push (cons name type) hyps)))))
    (nreverse hyps)))

(defun causal-catcolab--scan-goal ()
  "Extract the current proof goal from the goals buffer or point."
  (let ((goals-buf (get-buffer "*Goals*")))
    (if goals-buf
        (with-current-buffer goals-buf
          (when (re-search-forward "⊢\\s-*\\(.+\\)" nil t)
            (string-trim (match-string 1))))
      "goal")))

;;;###autoload
(defun causal-catcolab-status ()
  "Check CatColab backend status."
  (interactive)
  (condition-case _
      (progn
        (causal-catcolab-search-refs nil 1)
        (message "CatColab: Running at %s" causal-catcolab-url))
    (error (message "CatColab: Unreachable at %s" causal-catcolab-url))))

;;; Status Label

(defun causal-catcolab--status-label ()
  "Return short status string for Transient header."
  (format "CatColab @ %s%s"
          causal-catcolab-url
          (if causal-catcolab-token " [auth]" " [anon]")))

;;; Sub-menus

(transient-define-prefix causal-catcolab-new-tmenu ()
  "Create new CatColab document."
  [:description "New document"
   [("o" "Simple Olog" causal-catcolab-new-olog)
    ("c" "Causal Loop Diagram" causal-catcolab-new-causal-loop)
    ("p" "Petri Net" causal-catcolab-new-petri-net)]
   [("P" "Proof → Olog (from current buffer)" causal-catcolab-save-proof-as-olog)]]
  [:class transient-row
   (causal-lib-quit-one)])

(transient-define-prefix causal-catcolab-settings-tmenu ()
  "CatColab connection settings."
  ["Settings"
   ("u" "Backend URL" causal-catcolab--set-url)
   ("t" "Auth token" causal-catcolab--set-token)
   ("l" "Use local (localhost:8000)" causal-catcolab--use-local)
   ("r" "Use remote (backend.catcolab.org)" causal-catcolab--use-remote)]
  [:class transient-row
   (causal-lib-quit-one)])

(defun causal-catcolab--set-url ()
  "Set `causal-catcolab-url' interactively."
  (interactive)
  (setq causal-catcolab-url
        (read-string "CatColab backend URL: " causal-catcolab-url))
  (message "CatColab URL: %s" causal-catcolab-url))

(defun causal-catcolab--set-token ()
  "Set `causal-catcolab-token' interactively."
  (interactive)
  (setq causal-catcolab-token
        (let ((t (read-string "Firebase token (empty to clear): ")))
          (if (string-empty-p t) nil t)))
  (message "Token %s" (if causal-catcolab-token "set" "cleared")))

(defun causal-catcolab--use-local ()
  "Switch to local CatColab instance."
  (interactive)
  (setq causal-catcolab-url "http://localhost:8000")
  (message "CatColab: using localhost:8000"))

(defun causal-catcolab--use-remote ()
  "Switch to catcolab.org backend."
  (interactive)
  (setq causal-catcolab-url "https://backend.catcolab.org")
  (message "CatColab: using backend.catcolab.org"))

;;; Main Menu

;;;###autoload (autoload 'causal-catcolab-tmenu "causal-catcolab" nil t)

(transient-define-prefix causal-catcolab-tmenu ()
  "Causal CatColab — categorical modeling from Emacs."

  [:description causal-catcolab--status-label]

  [["Browse"
    ("b" "Browse public docs" causal-catcolab-browse)
    ("s" "Search by name…" causal-catcolab-browse)
    ("v" "View ref by UUID…" causal-catcolab-view-ref)
    ("o" "Open in browser…" causal-catcolab-open-in-browser)]

   ["Create"
    ("n" "New›" causal-catcolab-new-tmenu)
    ("P" "Proof → Olog" causal-catcolab-save-proof-as-olog)]

   ["Meta"
    ("?" "Status check" causal-catcolab-status)
    ("," "Settings›" causal-catcolab-settings-tmenu)]]

  [:class transient-row
   (causal-lib-quit-one)
   (causal-lib-quit-all)])

(provide 'causal-catcolab)
;;; causal-catcolab.el ends here
