;;; causal-self-walker-paperproof.el --- Paperproof visualization for walk chains -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, languages
;; URL: https://github.com/plurigrid/causal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Paperproof integration for the causal-self-walker pipeline.
;;
;; Converts walk state chains into paperproof-style visualizations:
;;   - Hypotheses → green nodes (available facts in scope)
;;   - Goals → red nodes (targets to prove)
;;   - Tactics → transparent nodes with dashed borders
;;   - Scope nesting via step ordering
;;
;; Supports two output modes:
;;   1. Buffer visualization — ASCII proof tree in Emacs
;;   2. JSON export — paperproof-compatible JSON for external rendering
;;   3. HTML export — self-contained HTML with inline CSS
;;
;; GF(3) triad:
;;   paperproof (-1) ⊗ self-walker (0) ⊗ lean-proof-walk (+1) = 0
;;
;; QUICK START
;;   (require 'causal-self-walker-paperproof)
;;   ;; After a walk: M-x causal-self-walker-paperproof-view

;;; Code:

(require 'cl-lib)
(require 'json)

(declare-function causal-self-walker-state-step "causal-self-walker")
(declare-function causal-self-walker-state-goal "causal-self-walker")
(declare-function causal-self-walker-state-tactic "causal-self-walker")
(declare-function causal-self-walker-state-hypotheses "causal-self-walker")
(declare-function causal-self-walker-state-status "causal-self-walker")
(declare-function causal-self-walker-state-source-fn "causal-self-walker")
(declare-function causal-self-walker--chain-ordered "causal-self-walker")
(declare-function causal-self-walker--format-hyps "causal-self-walker")

;;; Customization

(defgroup causal-self-walker-paperproof nil
  "Paperproof visualization for self-walker."
  :group 'causal-self-walker
  :prefix "causal-self-walker-paperproof-")

(defcustom causal-self-walker-paperproof-export-dir
  (expand-file-name "~/i/causal/exports/paperproof/")
  "Directory for exported paperproof files."
  :type 'directory
  :group 'causal-self-walker-paperproof)

(defcustom causal-self-walker-paperproof-color-hypotheses "#4ade80"
  "Color for hypothesis nodes (green)."
  :type 'string
  :group 'causal-self-walker-paperproof)

(defcustom causal-self-walker-paperproof-color-goals "#f87171"
  "Color for goal nodes (red)."
  :type 'string
  :group 'causal-self-walker-paperproof)

(defcustom causal-self-walker-paperproof-color-tactics "#e5e7eb"
  "Color for tactic nodes (light gray / transparent)."
  :type 'string
  :group 'causal-self-walker-paperproof)

;;; ── State to node conversion ─────────────────────────────────────────

(defun causal-self-walker-paperproof--hyp-nodes (hyps step-id)
  "Convert HYPS to paperproof hypothesis node alists.
STEP-ID is used for scoping."
  (let ((idx 0))
    (mapcar (lambda (h)
              (cl-incf idx)
              `((id . ,(format "hyp-%d-%d" step-id idx))
                (type . "hypothesis")
                (name . ,(car h))
                (value . ,(cdr h))
                (step . ,step-id)
                (color . ,causal-self-walker-paperproof-color-hypotheses)))
            hyps)))

(defun causal-self-walker-paperproof--goal-node (goal step-id)
  "Convert GOAL string to a paperproof goal node alist.
STEP-ID is used for scoping."
  `((id . ,(format "goal-%d" step-id))
    (type . "goal")
    (value . ,goal)
    (step . ,step-id)
    (color . ,causal-self-walker-paperproof-color-goals)))

(defun causal-self-walker-paperproof--tactic-node (tactic source step-id)
  "Convert TACTIC and SOURCE function to a paperproof tactic node.
STEP-ID is used for scoping."
  `((id . ,(format "tactic-%d" step-id))
    (type . "tactic")
    (name . ,tactic)
    (source . ,source)
    (step . ,step-id)
    (color . ,causal-self-walker-paperproof-color-tactics)))

;;; ── Chain to paperproof structure ────────────────────────────────────

(defun causal-self-walker-paperproof--state-to-box (state)
  "Convert a single walk STATE to a paperproof box structure.
Each box contains hypothesis nodes, a goal node, and a tactic node."
  (let* ((step (causal-self-walker-state-step state))
         (hyps (causal-self-walker-state-hypotheses state))
         (goal (causal-self-walker-state-goal state))
         (tactic (causal-self-walker-state-tactic state))
         (source (causal-self-walker-state-source-fn state))
         (status (causal-self-walker-state-status state)))
    `((id . ,(format "step-%d" step))
      (step . ,step)
      (status . ,(symbol-name status))
      (hypotheses . ,(vconcat (causal-self-walker-paperproof--hyp-nodes hyps step)))
      (goal . ,(causal-self-walker-paperproof--goal-node goal step))
      (tactic . ,(unless (string-empty-p tactic)
                   (causal-self-walker-paperproof--tactic-node tactic source step))))))

(defun causal-self-walker-paperproof-chain-to-json (chain &optional name)
  "Convert a walk CHAIN to a paperproof JSON structure.
NAME is an optional proof name for the export."
  (let* ((ordered (if (functionp 'causal-self-walker--chain-ordered)
                      (funcall 'causal-self-walker--chain-ordered)
                    (reverse chain)))
         (boxes (mapcar #'causal-self-walker-paperproof--state-to-box ordered))
         (edges (causal-self-walker-paperproof--edges ordered))
         (final (car (last ordered)))
         (completed (and final
                         (eq (causal-self-walker-state-status final) 'qed))))
    `((version . "1.0.0")
      (name . ,(or name "self-walker proof"))
      (completed . ,(if completed t :json-false))
      (steps . ,(length ordered))
      (boxes . ,(vconcat boxes))
      (edges . ,(vconcat edges)))))

(defun causal-self-walker-paperproof--edges (chain)
  "Generate edge alists connecting consecutive states in CHAIN."
  (let (edges)
    (when (> (length chain) 1)
      (cl-loop for i from 0 below (1- (length chain))
               for from-state = (nth i chain)
               for to-state = (nth (1+ i) chain)
               for from-step = (causal-self-walker-state-step from-state)
               for to-step = (causal-self-walker-state-step to-state)
               for tactic = (causal-self-walker-state-tactic to-state)
               do (push `((from . ,(format "step-%d" from-step))
                          (to . ,(format "step-%d" to-step))
                          (tactic . ,tactic))
                        edges)))
    (nreverse edges)))

;;; ── Buffer visualization ─────────────────────────────────────────────

(defun causal-self-walker-paperproof--pad (text width)
  "Right-pad TEXT with spaces to WIDTH characters."
  (let ((len (length text)))
    (if (>= len width) text
      (concat text (make-string (- width len) ?\s)))))

(defun causal-self-walker-paperproof--render-box (box)
  "Render a single paperproof BOX as ASCII art."
  (let* ((step (alist-get 'step box))
         (status (alist-get 'status box))
         (hyps (alist-get 'hypotheses box))
         (goal (alist-get 'goal box))
         (tactic (alist-get 'tactic box))
         (width 60)
         (border (make-string width ?─))
         (dashed (make-string width ?╌)))
    (concat
     (format "┌%s┐\n" border)
     (format "│ %s│\n"
             (causal-self-walker-paperproof--pad
              (format "Step %d [%s]" step status) (1- width)))
     (format "├%s┤\n" border)
     ;; Hypotheses
     (if (> (length hyps) 0)
         (concat
          (format "│ %s│\n"
                  (causal-self-walker-paperproof--pad "Hypotheses:" (1- width)))
          (mapconcat
           (lambda (h)
             (let ((text (format "  %s : %s" (alist-get 'name h) (alist-get 'value h))))
               (format "│ %s│\n"
                       (causal-self-walker-paperproof--pad text (1- width)))))
           (append hyps nil) ""))
       (format "│ %s│\n"
               (causal-self-walker-paperproof--pad "(no hypotheses)" (1- width))))
     ;; Goal
     (format "├%s┤\n" border)
     (let ((goal-text (or (alist-get 'value goal) "(no goal)")))
       (format "│ %s│\n"
               (causal-self-walker-paperproof--pad
                (format "Goal: %s" goal-text) (1- width))))
     ;; Tactic
     (when tactic
       (let ((tac-text (alist-get 'name tactic)))
         (concat
          (format "├%s┤\n" (substring dashed 0 (min (length dashed) width)))
          (format "│ %s│\n"
                  (causal-self-walker-paperproof--pad
                   (format "Tactic: %s" tac-text) (1- width))))))
     (format "└%s┘\n" border))))

(defun causal-self-walker-paperproof--render-edge ()
  "Render an edge connector between boxes."
  (concat
   (make-string 30 ?\s) "│\n"
   (make-string 30 ?\s) "▼\n"))

(defun causal-self-walker-paperproof-render (proof-json)
  "Render PROOF-JSON as an ASCII paperproof tree string."
  (let* ((name (alist-get 'name proof-json))
         (completed (alist-get 'completed proof-json))
         (steps (alist-get 'steps proof-json))
         (boxes (append (alist-get 'boxes proof-json) nil)))
    (concat
     (format "Paperproof: %s\n" name)
     (format "Status: %s  |  Steps: %d\n"
             (if (eq completed t) "QED" "incomplete") steps)
     (make-string 62 ?═) "\n\n"
     (mapconcat
      (lambda (box)
        (causal-self-walker-paperproof--render-box box))
      boxes
      (causal-self-walker-paperproof--render-edge)))))

;;; ── Interactive commands ─────────────────────────────────────────────

;;;###autoload
(defun causal-self-walker-paperproof-view ()
  "Display the current walk chain as a paperproof visualization."
  (interactive)
  (unless (boundp 'causal-self-walker--chain)
    (user-error "No walk chain. Run M-x causal-self-walker-run first"))
  (unless (symbol-value 'causal-self-walker--chain)
    (user-error "Walk chain is empty. Run M-x causal-self-walker-run first"))
  (let* ((chain (symbol-value 'causal-self-walker--chain))
         (name (format "Walk: %s" (buffer-name)))
         (proof-json (causal-self-walker-paperproof-chain-to-json chain name))
         (rendered (causal-self-walker-paperproof-render proof-json))
         (buf (get-buffer-create "*paperproof*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert rendered))
      (view-mode 1)
      (goto-char (point-min)))
    (display-buffer buf)))

;;;###autoload
(defun causal-self-walker-paperproof-export-json ()
  "Export the current walk chain as paperproof JSON."
  (interactive)
  (unless (boundp 'causal-self-walker--chain)
    (user-error "No walk chain"))
  (unless (symbol-value 'causal-self-walker--chain)
    (user-error "Walk chain is empty"))
  (let* ((chain (symbol-value 'causal-self-walker--chain))
         (name (read-string "Proof name: " (format "Walk: %s" (buffer-name))))
         (proof-json (causal-self-walker-paperproof-chain-to-json chain name))
         (dir (file-name-as-directory causal-self-walker-paperproof-export-dir))
         (filename (expand-file-name
                    (format "%s.json"
                            (replace-regexp-in-string
                             "[^a-zA-Z0-9_-]" "-"
                             (downcase name)))
                    dir))
         (json-str (json-encode proof-json)))
    (make-directory dir t)
    (with-temp-file filename
      (insert json-str))
    (message "Paperproof JSON exported to %s" filename)
    filename))

;;;###autoload
(defun causal-self-walker-paperproof-export-html ()
  "Export the current walk chain as a self-contained paperproof HTML file."
  (interactive)
  (unless (boundp 'causal-self-walker--chain)
    (user-error "No walk chain"))
  (unless (symbol-value 'causal-self-walker--chain)
    (user-error "Walk chain is empty"))
  (let* ((chain (symbol-value 'causal-self-walker--chain))
         (name (read-string "Proof name: " (format "Walk: %s" (buffer-name))))
         (proof-json (causal-self-walker-paperproof-chain-to-json chain name))
         (dir (file-name-as-directory causal-self-walker-paperproof-export-dir))
         (filename (expand-file-name
                    (format "%s.html"
                            (replace-regexp-in-string
                             "[^a-zA-Z0-9_-]" "-"
                             (downcase name)))
                    dir))
         (html (causal-self-walker-paperproof--generate-html proof-json)))
    (make-directory dir t)
    (with-temp-file filename
      (insert html))
    (message "Paperproof HTML exported to %s" filename)
    (when (y-or-n-p "Open in browser? ")
      (browse-url (concat "file://" filename)))
    filename))

;;; ── HTML generation ──────────────────────────────────────────────────

(defun causal-self-walker-paperproof--generate-html (proof-json)
  "Generate a self-contained HTML document from PROOF-JSON."
  (let* ((name (alist-get 'name proof-json))
         (completed (alist-get 'completed proof-json))
         (boxes (append (alist-get 'boxes proof-json) nil))
         (json-str (json-encode proof-json)))
    (concat
     "<!DOCTYPE html>\n"
     "<html lang=\"en\">\n"
     "<head>\n"
     "  <meta charset=\"UTF-8\">\n"
     "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
     (format "  <title>Paperproof: %s</title>\n" (causal-self-walker-paperproof--html-escape name))
     "  <style>\n"
     (causal-self-walker-paperproof--css)
     "  </style>\n"
     "</head>\n"
     "<body>\n"
     (format "  <h1>%s</h1>\n" (causal-self-walker-paperproof--html-escape name))
     (format "  <p class=\"status\">%s</p>\n"
             (if (eq completed t) "QED &#10003;" "In Progress..."))
     "  <div class=\"proof-tree\">\n"
     (mapconcat #'causal-self-walker-paperproof--html-box boxes
                "    <div class=\"edge\">&#9660;</div>\n")
     "  </div>\n"
     "  <script>\n"
     "    // Paperproof data for external tools\n"
     (format "    window.paperproofData = %s;\n" json-str)
     "  </script>\n"
     "</body>\n"
     "</html>\n")))

(defun causal-self-walker-paperproof--css ()
  "Return CSS for the paperproof HTML export."
  (format
   "    * { margin: 0; padding: 0; box-sizing: border-box; }
    body { font-family: 'SF Mono', 'Cascadia Code', monospace; background: #1a1a2e; color: #e0e0e0; padding: 2rem; }
    h1 { color: #fff; margin-bottom: 0.5rem; }
    .status { color: #a0a0a0; margin-bottom: 2rem; font-size: 0.9rem; }
    .proof-tree { display: flex; flex-direction: column; align-items: center; gap: 0; }
    .edge { color: #666; font-size: 1.2rem; text-align: center; margin: 0.25rem 0; }
    .box { border: 2px solid #333; border-radius: 8px; width: 500px; overflow: hidden; background: #16213e; }
    .box-header { padding: 0.5rem 1rem; background: #0f3460; font-weight: bold; display: flex; justify-content: space-between; }
    .hyp-section { padding: 0.5rem 1rem; border-top: 1px solid #333; }
    .hyp-section h3 { color: %s; font-size: 0.8rem; margin-bottom: 0.25rem; }
    .hyp { color: %s; padding: 0.15rem 0; font-size: 0.85rem; }
    .goal-section { padding: 0.5rem 1rem; border-top: 1px solid #333; }
    .goal-section h3 { color: %s; font-size: 0.8rem; margin-bottom: 0.25rem; }
    .goal { color: %s; font-size: 0.9rem; }
    .tactic-section { padding: 0.5rem 1rem; border-top: 2px dashed #555; background: rgba(255,255,255,0.03); }
    .tactic-section h3 { color: #999; font-size: 0.8rem; margin-bottom: 0.25rem; }
    .tactic { color: #ccc; font-size: 0.85rem; }
    .badge { font-size: 0.7rem; padding: 0.1rem 0.4rem; border-radius: 4px; }
    .badge-qed { background: #065f46; color: #6ee7b7; }
    .badge-active { background: #1e3a5f; color: #93c5fd; }
    .badge-initial { background: #3f3f46; color: #a1a1aa; }
    .badge-rejected { background: #7f1d1d; color: #fca5a5; }
"
   causal-self-walker-paperproof-color-hypotheses
   causal-self-walker-paperproof-color-hypotheses
   causal-self-walker-paperproof-color-goals
   causal-self-walker-paperproof-color-goals))

(defun causal-self-walker-paperproof--html-escape (str)
  "Escape STR for safe HTML insertion."
  (let ((s (or str "")))
    (setq s (replace-regexp-in-string "&" "&amp;" s))
    (setq s (replace-regexp-in-string "<" "&lt;" s))
    (setq s (replace-regexp-in-string ">" "&gt;" s))
    (setq s (replace-regexp-in-string "\"" "&quot;" s))
    s))

(defun causal-self-walker-paperproof--html-box (box)
  "Render a single BOX alist as an HTML div."
  (let* ((step (alist-get 'step box))
         (status (alist-get 'status box))
         (hyps (append (alist-get 'hypotheses box) nil))
         (goal (alist-get 'goal box))
         (tactic (alist-get 'tactic box))
         (badge-class (cond ((string= status "qed") "badge-qed")
                            ((string= status "active") "badge-active")
                            ((string= status "rejected") "badge-rejected")
                            (t "badge-initial"))))
    (concat
     "    <div class=\"box\">\n"
     (format "      <div class=\"box-header\"><span>Step %d</span><span class=\"badge %s\">%s</span></div>\n"
             step badge-class (upcase status))
     ;; Hypotheses
     "      <div class=\"hyp-section\">\n"
     "        <h3>Hypotheses</h3>\n"
     (if (> (length hyps) 0)
         (mapconcat
          (lambda (h)
            (format "        <div class=\"hyp\">%s : %s</div>\n"
                    (causal-self-walker-paperproof--html-escape (alist-get 'name h))
                    (causal-self-walker-paperproof--html-escape (alist-get 'value h))))
          hyps "")
       "        <div class=\"hyp\">(none)</div>\n")
     "      </div>\n"
     ;; Goal
     "      <div class=\"goal-section\">\n"
     "        <h3>Goal</h3>\n"
     (format "        <div class=\"goal\">%s</div>\n"
             (causal-self-walker-paperproof--html-escape (or (alist-get 'value goal) "")))
     "      </div>\n"
     ;; Tactic
     (when tactic
       (concat
        "      <div class=\"tactic-section\">\n"
        "        <h3>Tactic</h3>\n"
        (format "        <div class=\"tactic\">%s</div>\n"
                (causal-self-walker-paperproof--html-escape (alist-get 'name tactic)))
        "      </div>\n"))
     "    </div>\n")))

;;; ── Utility: chain to flat node list ─────────────────────────────────

(defun causal-self-walker-paperproof-chain-nodes (chain)
  "Extract all nodes (hyps, goals, tactics) from CHAIN as a flat list."
  (let* ((ordered (if (functionp 'causal-self-walker--chain-ordered)
                      (funcall 'causal-self-walker--chain-ordered)
                    (reverse chain)))
         nodes)
    (dolist (state ordered)
      (let* ((step (causal-self-walker-state-step state))
             (hyps (causal-self-walker-state-hypotheses state))
             (goal (causal-self-walker-state-goal state))
             (tactic (causal-self-walker-state-tactic state))
             (source (causal-self-walker-state-source-fn state)))
        (dolist (hn (causal-self-walker-paperproof--hyp-nodes hyps step))
          (push hn nodes))
        (push (causal-self-walker-paperproof--goal-node goal step) nodes)
        (unless (string-empty-p tactic)
          (push (causal-self-walker-paperproof--tactic-node tactic source step) nodes))))
    (nreverse nodes)))

(provide 'causal-self-walker-paperproof)
;;; causal-self-walker-paperproof.el ends here
