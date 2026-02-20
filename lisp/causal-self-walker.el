;;; causal-self-walker.el --- Self-walking proof pipeline -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, languages, categories
;; URL: https://github.com/plurigrid/causal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Self-Walker: an introspective automation for the causal-proof /
;; causal-proofreader + ProofGeneral/Narya stack.
;;
;; It reads its own Elisp source to discover available tactics,
;; walks proof states step-by-step, records a state chain, and
;; exports the terminal proof state as a CatColab simple-olog.
;;
;; GF(3) Triad:
;;   self-walker (0) ⊗ proofgeneral-narya (-1) ⊗ lean-proof-walk (+1) = 0
;;
;; QUICK START
;;   (require 'causal-self-walker)
;;   M-x causal-self-walker-tmenu
;;
;; Or from causal-proof-tmenu:  S → Self-walk›

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'causal-lib)
(require 'causal-self-walker-mc nil t)

;;; Customization

(defgroup causal-self-walker nil
  "Self-walking proof pipeline."
  :group 'causal
  :prefix "causal-self-walker-")

(defcustom causal-self-walker-max-steps 50
  "Maximum proof steps before the walker halts."
  :type 'integer
  :group 'causal-self-walker)

(defcustom causal-self-walker-max-retries 3
  "Maximum tactic retries per step before backtracking."
  :type 'integer
  :group 'causal-self-walker)

(defcustom causal-self-walker-use-mc nil
  "When non-nil, use abductive Monte Carlo tactic proposals during walk."
  :type 'boolean
  :group 'causal-self-walker)

(defcustom causal-self-walker-mc-top-k 3
  "Number of top MC proposals to try per step."
  :type 'integer
  :group 'causal-self-walker)

(defcustom causal-self-walker-auto-export t
  "When non-nil, automatically offer CatColab olog export on QED."
  :type 'boolean
  :group 'causal-self-walker)

;;; State Chain

(cl-defstruct (causal-self-walker-state
               (:constructor causal-self-walker-state-create))
  "A single proof state in the walk chain."
  (step 0 :type integer)
  (hypotheses nil :type list)
  (goal "" :type string)
  (tactic "" :type string)
  (source-fn "" :type string)
  (status 'active :type symbol))

(defvar causal-self-walker--chain nil
  "Current walk chain: list of `causal-self-walker-state' structs.")

(defvar causal-self-walker--seed 0
  "Walk seed for deterministic behavior. XOR'd with state hashes.")

(defvar causal-self-walker--tactic-vocab nil
  "Cached tactic vocabulary discovered from source.")

;;; Tactic Introspection

(defun causal-self-walker--discover-tactics-from-file (file)
  "Scan FILE for tactic insertion commands.
Returns a list of tactic name strings."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (tactics)
        (while (re-search-forward
                "--insert-tactic\\s-+\"\\([^\"]+\\)\"" nil t)
          (let ((tac (match-string 1)))
            (unless (member tac tactics)
              (push tac tactics))))
        (nreverse tactics)))))

(defun causal-self-walker-discover-tactics ()
  "Discover tactic vocabulary by scanning causal Elisp source files.
Scans `causal-proof.el' and `causal-proofreader.el' for insert-tactic calls.
Returns a deduplicated list of tactic name strings."
  (let* ((proof-file (locate-library "causal-proof"))
         (reader-file (locate-library "causal-proofreader"))
         (from-proof (causal-self-walker--discover-tactics-from-file proof-file))
         (from-reader (causal-self-walker--discover-tactics-from-file reader-file))
         (all (append from-proof from-reader))
         (seen (make-hash-table :test 'equal))
         result)
    (dolist (tac all)
      (unless (gethash tac seen)
        (puthash tac t seen)
        (push tac result)))
    (setq causal-self-walker--tactic-vocab (nreverse result))))

(defun causal-self-walker-tactic-vocab ()
  "Return the current tactic vocabulary, discovering if needed."
  (or causal-self-walker--tactic-vocab
      (causal-self-walker-discover-tactics)))

;;; State Scanning

(defun causal-self-walker--scan-hypotheses ()
  "Read current hypotheses from the *Goals* buffer or current buffer.
Returns list of (name . type) pairs."
  (if (fboundp 'causal-catcolab--scan-hypotheses)
      (causal-catcolab--scan-hypotheses)
    (causal-self-walker--parse-hypotheses-fallback)))

(defun causal-self-walker--parse-hypotheses-fallback ()
  "Fallback hypothesis parser when causal-catcolab is not loaded."
  (let ((buf (get-buffer "*Goals*")))
    (when buf
      (with-current-buffer buf
        (let (hyps)
          (goto-char (point-min))
          (while (re-search-forward
                  "^\\s-*\\([[:alnum:]_']+\\)\\s-*:\\s-*\\([^⊢\n]+\\)" nil t)
            (push (cons (match-string 1) (string-trim (match-string 2))) hyps))
          (nreverse hyps))))))

(defun causal-self-walker--scan-goal ()
  "Read the current proof goal from the *Goals* buffer."
  (if (fboundp 'causal-catcolab--scan-goal)
      (causal-catcolab--scan-goal)
    (let ((buf (get-buffer "*Goals*")))
      (when buf
        (with-current-buffer buf
          (goto-char (point-min))
          (if (re-search-forward "⊢\\s-*\\(.+\\)" nil t)
              (string-trim (match-string 1))
            ""))))))

(defun causal-self-walker--no-goals-p (goal)
  "Return non-nil if GOAL indicates proof is complete."
  (or (null goal)
      (string-empty-p goal)
      (string-match-p "\\(?:No\\s-+\\(?:more\\s-+\\)?goals\\|QED\\|No Goals\\)"
                      goal)))

;;; Seed Evolution

(defun causal-self-walker--evolve-seed (state)
  "Evolve the walk seed by XOR with a hash of STATE."
  (let ((h (sxhash (format "%S" state))))
    (setq causal-self-walker--seed
          (logxor causal-self-walker--seed h))))

;;; Chain Recording

(defun causal-self-walker--record-state (step hyps goal tactic source-fn status)
  "Record a proof state in the walk chain."
  (let ((state (causal-self-walker-state-create
                :step step
                :hypotheses hyps
                :goal goal
                :tactic tactic
                :source-fn source-fn
                :status status)))
    (push state causal-self-walker--chain)
    (causal-self-walker--evolve-seed state)
    state))

(defun causal-self-walker--chain-ordered ()
  "Return the walk chain in chronological order."
  (reverse causal-self-walker--chain))

;;; Chain Formatting

(defun causal-self-walker--format-hyps (hyps)
  "Format HYPS as a comma-separated context string."
  (if (null hyps)
      ""
    (mapconcat (lambda (h) (format "%s : %s" (car h) (cdr h)))
               hyps ", ")))

(defun causal-self-walker--format-state (state)
  "Format a single STATE as a human-readable string."
  (let ((step (causal-self-walker-state-step state))
        (hyps (causal-self-walker-state-hypotheses state))
        (goal (causal-self-walker-state-goal state))
        (tactic (causal-self-walker-state-tactic state))
        (source (causal-self-walker-state-source-fn state))
        (status (causal-self-walker-state-status state)))
    (concat
     (format "State %d: %s ⊢ %s\n"
             step
             (causal-self-walker--format-hyps hyps)
             (or goal "(no goal)"))
     (unless (string-empty-p tactic)
       (format "\n  tactic: %s\n  via: %s\n  status: %s\n"
               tactic source status)))))

(defun causal-self-walker--format-chain ()
  "Format the entire walk chain as a string."
  (let ((chain (causal-self-walker--chain-ordered)))
    (concat
     (format "Self-Walker State Chain (%d steps)\n" (length chain))
     (format "Seed: #x%X\n" causal-self-walker--seed)
     (make-string 60 ?─)
     "\n\n"
     (mapconcat #'causal-self-walker--format-state chain "\n"))))

;;; Walk Display

(defun causal-self-walker--display-chain ()
  "Display the walk chain in a dedicated buffer."
  (let ((buf (get-buffer-create "*self-walker*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (causal-self-walker--format-chain)))
      (view-mode 1)
      (goto-char (point-min)))
    (display-buffer buf)))

;;; Walk Engine

(defun causal-self-walker--init ()
  "Initialize a new walk. Resets chain and seed."
  (setq causal-self-walker--chain nil)
  (let* ((hyps (causal-self-walker--scan-hypotheses))
         (goal (causal-self-walker--scan-goal)))
    (setq causal-self-walker--seed
          (logxor (sxhash (or goal "")) #x9E3779B97F4A7C15))
    (causal-self-walker--record-state 0 hyps goal "" "" 'initial)
    (causal-self-walker-tactic-vocab)))

(defun causal-self-walker--try-step-forward ()
  "Attempt to step forward via the proof backend.
Returns non-nil on success."
  (condition-case nil
      (progn
        (if (fboundp 'causal-proof-step-forward)
            (causal-proof-step-forward)
          (when (fboundp 'causal-proofreader-step-forward)
            (causal-proofreader-step-forward)))
        t)
    (error nil)))

(defun causal-self-walker--try-step-backward ()
  "Attempt to step backward via the proof backend."
  (condition-case nil
      (progn
        (if (fboundp 'causal-proof-step-backward)
            (causal-proof-step-backward)
          (when (fboundp 'causal-proofreader-step-backward)
            (causal-proofreader-step-backward)))
        t)
    (error nil)))

;;; MC Integration

(defun causal-self-walker--mc-available-p ()
  "Return non-nil if the MC module is loaded."
  (featurep 'causal-self-walker-mc))

(defun causal-self-walker--mc-step (step)
  "Attempt a step using MC-ranked tactic proposals.
Returns the tactic name on success, nil on failure."
  (when (causal-self-walker--mc-available-p)
    (let* ((goal (causal-self-walker--scan-goal))
           (vocab (causal-self-walker-tactic-vocab))
           (proposals (causal-self-walker-mc-propose
                       goal vocab causal-self-walker--seed))
           (top-k (cl-subseq proposals 0
                             (min causal-self-walker-mc-top-k
                                  (length proposals))))
           (accepted nil))
      (cl-loop for entry in top-k
               for tactic = (car entry)
               for score = (cdr entry)
               until accepted
               do (let ((ok (causal-self-walker--try-step-forward)))
                    (when ok
                      (setq accepted tactic))))
      accepted)))

;;;###autoload
(defun causal-self-walker-run ()
  "Run the self-walker on the current proof buffer.
Steps through the proof, recording each state transition,
and exports the result as a CatColab olog on completion.
When `causal-self-walker-use-mc' is non-nil and the MC module
is loaded, uses abductive Monte Carlo tactic proposals."
  (interactive)
  (message "Self-walker: initializing%s..."
           (if (and causal-self-walker-use-mc
                    (causal-self-walker--mc-available-p))
               " (MC mode)" ""))
  (causal-self-walker--init)
  (let ((step 0)
        (max-steps causal-self-walker-max-steps)
        (use-mc (and causal-self-walker-use-mc
                     (causal-self-walker--mc-available-p)))
        (done nil))
    (while (and (not done) (< step max-steps))
      (cl-incf step)
      (let ((accepted (if use-mc
                          (causal-self-walker--mc-step step)
                        (causal-self-walker--try-step-forward))))
        (if accepted
            (let* ((hyps (causal-self-walker--scan-hypotheses))
                   (goal (causal-self-walker--scan-goal))
                   (tactic-name (if (stringp accepted) accepted "step-forward"))
                   (source-fn (if use-mc "mc-propose" "causal-proof-step-forward")))
              (if (causal-self-walker--no-goals-p goal)
                  (progn
                    (causal-self-walker--record-state
                     step hyps "No Goals" tactic-name source-fn 'qed)
                    (setq done t))
                (causal-self-walker--record-state
                 step hyps goal tactic-name source-fn 'active)))
          (causal-self-walker--record-state
           step nil "" "step-forward" "causal-proof-step-forward" 'rejected)
          (setq done t))))
    (causal-self-walker--display-chain)
    (when (and causal-self-walker-auto-export
               (fboundp 'causal-catcolab-save-proof-as-olog)
               (eq (causal-self-walker-state-status (car causal-self-walker--chain))
                   'qed))
      (when (y-or-n-p "Proof complete! Export to CatColab olog? ")
        (causal-catcolab-save-proof-as-olog)))
    (message "Self-walker: %s after %d steps (seed #x%X)%s"
             (if done "finished" "halted (max steps)")
             step causal-self-walker--seed
             (if use-mc " [MC]" ""))))

;;;###autoload
(defun causal-self-walker-show-chain ()
  "Display the most recent walk chain."
  (interactive)
  (if causal-self-walker--chain
      (causal-self-walker--display-chain)
    (message "No walk chain recorded yet. Run M-x causal-self-walker-run first.")))

;;;###autoload
(defun causal-self-walker-show-vocab ()
  "Display the discovered tactic vocabulary."
  (interactive)
  (let ((vocab (causal-self-walker-tactic-vocab)))
    (with-current-buffer (get-buffer-create "*self-walker: tactics*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Self-Walker Tactic Vocabulary (%d tactics)\n"
                        (length vocab)))
        (insert (make-string 40 ?─) "\n\n")
        (dolist (tac vocab)
          (insert (format "  %s\n" tac))))
      (view-mode 1)
      (goto-char (point-min)))
    (display-buffer "*self-walker: tactics*")))

;;;###autoload
(defun causal-self-walker-export-olog ()
  "Export the last walk chain as a CatColab olog."
  (interactive)
  (unless causal-self-walker--chain
    (user-error "No walk chain. Run M-x causal-self-walker-run first"))
  (if (fboundp 'causal-catcolab-save-proof-as-olog)
      (causal-catcolab-save-proof-as-olog)
    (user-error "causal-catcolab not loaded")))

;;;###autoload
(defun causal-self-walker-export-chain-as-olog ()
  "Export the entire walk chain (not just final state) as a CatColab olog.
Each proof state becomes an olog object; transitions become morphisms."
  (interactive)
  (unless causal-self-walker--chain
    (user-error "No walk chain. Run M-x causal-self-walker-run first"))
  (unless (fboundp 'causal-catcolab-new-ref)
    (user-error "causal-catcolab not loaded"))
  (let* ((chain (causal-self-walker--chain-ordered))
         (name (read-string "Olog name: "
                            (format "Walk: %s" (buffer-name))))
         (objects (causal-self-walker--chain-to-ob-generators chain))
         (morphisms (causal-self-walker--chain-to-mor-generators chain))
         (content `(("type" . "model")
                    ("name" . ,name)
                    ("theory" . "simple-olog")
                    ("notebook" . (("tag" . "Notebook") ("cells" . [])))
                    ("content" . (("ob_generators" . ,objects)
                                  ("mor_generators" . ,morphisms)))))
         (ref-id (causal-catcolab-new-ref content)))
    (message "Walk olog created: %s" ref-id)
    (when (y-or-n-p "Open in browser? ")
      (browse-url (causal-catcolab-url-for-ref ref-id)))
    ref-id))

(defun causal-self-walker--chain-to-ob-generators (chain)
  "Convert CHAIN states to CatColab ob_generators vector."
  (vconcat
   (mapcar (lambda (state)
             (let ((step (causal-self-walker-state-step state))
                   (goal (causal-self-walker-state-goal state))
                   (hyps (causal-self-walker-state-hypotheses state)))
               `(("tag" . "ObType")
                 ("obType" . (("tag" . "Basic") ("content" . "Object")))
                 ("name" . ,(format "State_%d" step))
                 ("description" . ,(format "%s ⊢ %s"
                                           (causal-self-walker--format-hyps hyps)
                                           (or goal "")))
                 ("id" . ,(format "state-%d" step)))))
           chain)))

(defun causal-self-walker--chain-to-mor-generators (chain)
  "Convert CHAIN transitions to CatColab mor_generators vector."
  (let (morphisms)
    (when (> (length chain) 1)
      (cl-loop for i from 0 below (1- (length chain))
               for from-state = (nth i chain)
               for to-state = (nth (1+ i) chain)
               do (push `(("tag" . "MorType")
                          ("morType" . (("tag" . "Basic") ("content" . "Morphism")))
                          ("name" . ,(format "%s"
                                            (causal-self-walker-state-tactic to-state)))
                          ("description" . ,(causal-self-walker-state-source-fn to-state))
                          ("id" . ,(format "step-%d-to-%d"
                                           (causal-self-walker-state-step from-state)
                                           (causal-self-walker-state-step to-state)))
                          ("dom" . (("tag" . "Basic")
                                    ("content" . ,(format "state-%d"
                                                          (causal-self-walker-state-step from-state)))))
                          ("cod" . (("tag" . "Basic")
                                    ("content" . ,(format "state-%d"
                                                          (causal-self-walker-state-step to-state))))))
                        morphisms)))
    (vconcat (nreverse morphisms))))

;;; Reset

(defun causal-self-walker-reset ()
  "Reset the walker state."
  (interactive)
  (setq causal-self-walker--chain nil
        causal-self-walker--seed 0
        causal-self-walker--tactic-vocab nil)
  (message "Self-walker: reset."))

;;; Toggle

;;;###autoload
(defun causal-self-walker-toggle-mc ()
  "Toggle Monte Carlo tactic proposal mode."
  (interactive)
  (setq causal-self-walker-use-mc (not causal-self-walker-use-mc))
  (message "Self-walker MC mode: %s" (if causal-self-walker-use-mc "ON" "OFF")))

;;; Transient Menu

;;;###autoload (autoload 'causal-self-walker-tmenu "causal-self-walker" nil t)

(transient-define-prefix causal-self-walker-tmenu ()
  "Causal Self-Walker — introspective proof automation."

  [:description
   (lambda ()
     (format "Self-Walker [%d steps, seed #x%X]"
             (length causal-self-walker--chain)
             causal-self-walker--seed))]

  [["Walk"
    ("w" "Walk current proof" causal-self-walker-run)
    ("R" "Reset" causal-self-walker-reset)]

   ["Inspect"
    ("c" "Show chain" causal-self-walker-show-chain)
    ("v" "Show tactic vocab" causal-self-walker-show-vocab)]

   ["Monte Carlo"
    ("m" "MC proposals" causal-self-walker-mc-show-proposals
     :if (lambda () (causal-self-walker--mc-available-p)))
    ("M" "Toggle MC mode" causal-self-walker-toggle-mc
     :description (lambda ()
                    (format "MC mode: %s" (if causal-self-walker-use-mc "ON" "OFF"))))]

   ["Export"
    ("o" "Final state > Olog" causal-self-walker-export-olog
     :if (lambda () (and causal-self-walker--chain
                         (fboundp 'causal-catcolab-save-proof-as-olog))))
    ("O" "Full chain > Olog" causal-self-walker-export-chain-as-olog
     :if (lambda () (and causal-self-walker--chain
                         (fboundp 'causal-catcolab-new-ref))))]]

  [:class transient-row
   (causal-lib-quit-one)
   (causal-lib-quit-all)])

(provide 'causal-self-walker)
;;; causal-self-walker.el ends here
