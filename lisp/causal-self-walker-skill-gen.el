;;; causal-self-walker-skill-gen.el --- Generate ASI skills from walk paths -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, languages
;; URL: https://github.com/plurigrid/causal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Generate ASI skills from self-walker proof paths.
;;
;; Each completed walk yields a tactic sequence that becomes a reusable
;; proof strategy skill.  The skill gets:
;;   - A name derived from the goal and tactic signature
;;   - A GF(3) trit computed from the walk's tactic trit sum
;;   - SKILL.md in the asi/skills/ format
;;
;; GF(3) triad:
;;   self-walker-skill-gen (+1) ⊗ self-walker (0) ⊗ abductive-monte-carlo (-1) = 0

;;; Code:

(require 'cl-lib)

;; Avoid circular require.
(declare-function causal-self-walker-state-step "causal-self-walker")
(declare-function causal-self-walker-state-goal "causal-self-walker")
(declare-function causal-self-walker-state-tactic "causal-self-walker")
(declare-function causal-self-walker-state-hypotheses "causal-self-walker")
(declare-function causal-self-walker-state-status "causal-self-walker")
(declare-function causal-self-walker--chain-ordered "causal-self-walker")
(declare-function causal-self-walker--format-hyps "causal-self-walker")
(declare-function causal-self-walker-mc-tactic-trit "causal-self-walker-mc")
(declare-function causal-self-walker-mc-walk-trit-sum "causal-self-walker-mc")
(defvar causal-self-walker--chain)
(defvar causal-self-walker--seed)

;;; Customization

(defgroup causal-self-walker-skill-gen nil
  "ASI skill generation from walk paths."
  :group 'causal-self-walker
  :prefix "causal-self-walker-skill-gen-")

(defcustom causal-self-walker-skill-gen-output-dir nil
  "Directory to write generated SKILL.md files.
When nil, defaults to ~/i/asi/skills/."
  :type '(choice (const nil) directory)
  :group 'causal-self-walker-skill-gen)

;;; Skill Name Generation

(defun causal-self-walker-skill-gen--slugify (str)
  "Convert STR to a kebab-case slug suitable for a skill directory name."
  (let ((s (downcase (string-trim str))))
    (setq s (replace-regexp-in-string "[^a-z0-9]+" "-" s))
    (setq s (replace-regexp-in-string "^-+\\|-+$" "" s))
    (if (> (length s) 60)
        (substring s 0 60)
      s)))

(defun causal-self-walker-skill-gen--tactic-signature (chain)
  "Extract the ordered tactic sequence from CHAIN as a compact string."
  (let ((tactics (cl-remove-if
                  #'string-empty-p
                  (mapcar #'causal-self-walker-state-tactic chain))))
    (mapconcat #'identity tactics "-")))

(defun causal-self-walker-skill-gen--skill-name (chain)
  "Derive a skill name from the walk CHAIN.
Uses the initial goal slugified, suffixed with tactic count."
  (let* ((first-goal (cl-loop for s in chain
                              for g = (causal-self-walker-state-goal s)
                              when (and g (not (string-empty-p g))
                                        (not (string-match-p "No Goals" g)))
                              return g))
         (slug (causal-self-walker-skill-gen--slugify (or first-goal "proof")))
         (n-tactics (length (cl-remove-if
                             #'string-empty-p
                             (mapcar #'causal-self-walker-state-tactic chain)))))
    (format "%s-%d-step" slug n-tactics)))

;;; Trit Computation

(defun causal-self-walker-skill-gen--walk-trit (chain)
  "Compute the GF(3) trit for the walk described by CHAIN.
If causal-self-walker-mc is loaded, uses tactic trits.
Otherwise defaults to 0 (ergodic)."
  (if (fboundp 'causal-self-walker-mc-tactic-trit)
      (let ((sum (apply #'+
                        (mapcar (lambda (s)
                                  (causal-self-walker-mc-tactic-trit
                                   (causal-self-walker-state-tactic s)))
                                chain))))
        (let ((m (mod sum 3)))
          (cond ((= m 0) 0)
                ((= m 1) 1)
                ((= m 2) -1))))
    0))

(defun causal-self-walker-skill-gen--trit-label (trit)
  "Return human label for TRIT value."
  (cond ((= trit 1) "PLUS — generative strategy")
        ((= trit -1) "MINUS — reductive strategy")
        (t "ERGODIC — balanced strategy")))

;;; SKILL.md Generation

(defun causal-self-walker-skill-gen--format-skill-md (chain seed)
  "Generate SKILL.md content from walk CHAIN with SEED."
  (let* ((name (causal-self-walker-skill-gen--skill-name chain))
         (trit (causal-self-walker-skill-gen--walk-trit chain))
         (tactic-sig (causal-self-walker-skill-gen--tactic-signature chain))
         (first-state (car chain))
         (last-state (car (last chain)))
         (initial-goal (causal-self-walker-state-goal first-state))
         (n-steps (length chain))
         (tactics (cl-remove-if
                   #'string-empty-p
                   (mapcar #'causal-self-walker-state-tactic chain))))
    (concat
     ;; YAML frontmatter
     "---\n"
     (format "name: %s\n" name)
     (format "description: \"Proof strategy for %s using %d tactics: %s\"\n"
             (or initial-goal "goal") (length tactics) tactic-sig)
     "version: 0.1.0\n"
     "metadata:\n"
     (format "  trit: %d\n" trit)
     (format "  seed: \"0x%X\"\n" seed)
     "  source: causal-self-walker\n"
     "---\n\n"

     ;; Title
     (format "# %s\n\n" name)
     (format "> Auto-generated from self-walker proof path (seed 0x%X)\n\n" seed)
     (format "**Trit**: %d (%s)\n\n" trit (causal-self-walker-skill-gen--trit-label trit))

     ;; GF(3) triad
     "## GF(3) Triad\n\n"
     "```\n"
     (format "%s (%d) ⊗ self-walker (0) ⊗ abductive-monte-carlo (-1) = %d ✓\n"
             name trit (mod (+ trit 0 -1) 3))
     "```\n\n"

     ;; Strategy
     "## Tactic Sequence\n\n"
     "| Step | Tactic | Goal (before) |\n"
     "|------|--------|---------------|\n"
     (mapconcat
      (lambda (s)
        (let ((step (causal-self-walker-state-step s))
              (tactic (causal-self-walker-state-tactic s))
              (goal (causal-self-walker-state-goal s)))
          (unless (string-empty-p tactic)
            (format "| %d | `%s` | %s |"
                    step tactic
                    (truncate-string-to-width (or goal "") 50)))))
      chain "\n")
     "\n\n"

     ;; State chain
     "## State Chain\n\n"
     "```\n"
     (mapconcat
      (lambda (s)
        (let ((step (causal-self-walker-state-step s))
              (hyps (causal-self-walker-state-hypotheses s))
              (goal (causal-self-walker-state-goal s))
              (tactic (causal-self-walker-state-tactic s)))
          (concat
           (format "State %d: %s ⊢ %s"
                   step
                   (causal-self-walker--format-hyps hyps)
                   (or goal "(no goal)"))
           (unless (string-empty-p tactic)
             (format "\n  tactic: %s" tactic)))))
      chain "\n\n")
     "\n```\n\n"

     ;; Invocation
     "## Invocation\n\n"
     "From causal-self-walker transient menu:\n\n"
     "```\n"
     "C-o → Self-walk› → g → Generate ASI skill\n"
     "```\n\n"

     ;; Metadata
     "## Walk Metadata\n\n"
     (format "- **Steps**: %d\n" n-steps)
     (format "- **Tactics used**: %s\n" tactic-sig)
     (format "- **Seed**: `0x%X`\n" seed)
     (format "- **GF(3) trit**: %d\n" trit)
     )))

;;; File Output

(defun causal-self-walker-skill-gen--output-dir ()
  "Return the output directory for generated skills."
  (or causal-self-walker-skill-gen-output-dir
      (expand-file-name "~/i/asi/skills/")))

(defun causal-self-walker-skill-gen--write-skill (chain seed)
  "Write a SKILL.md for the walk CHAIN to the asi/skills directory.
Returns the path to the created file."
  (let* ((name (causal-self-walker-skill-gen--skill-name chain))
         (dir (expand-file-name name (causal-self-walker-skill-gen--output-dir)))
         (file (expand-file-name "SKILL.md" dir))
         (content (causal-self-walker-skill-gen--format-skill-md chain seed)))
    (make-directory dir t)
    (with-temp-file file
      (insert content))
    file))

;;; Interactive Commands

;;;###autoload
(defun causal-self-walker-skill-gen-preview ()
  "Preview the ASI skill that would be generated from the current walk chain."
  (interactive)
  (unless causal-self-walker--chain
    (user-error "No walk chain. Run M-x causal-self-walker-run first"))
  (let* ((chain (causal-self-walker--chain-ordered))
         (content (causal-self-walker-skill-gen--format-skill-md
                   chain causal-self-walker--seed)))
    (with-current-buffer (get-buffer-create "*self-walker: skill preview*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (when (fboundp 'markdown-mode) (markdown-mode)))
      (view-mode 1)
      (goto-char (point-min)))
    (display-buffer "*self-walker: skill preview*")))

;;;###autoload
(defun causal-self-walker-skill-gen-export ()
  "Generate and write an ASI skill from the current walk chain."
  (interactive)
  (unless causal-self-walker--chain
    (user-error "No walk chain. Run M-x causal-self-walker-run first"))
  (let* ((chain (causal-self-walker--chain-ordered))
         (name (causal-self-walker-skill-gen--skill-name chain))
         (dir (causal-self-walker-skill-gen--output-dir)))
    (when (y-or-n-p (format "Generate skill '%s' in %s? " name dir))
      (let ((file (causal-self-walker-skill-gen--write-skill
                   chain causal-self-walker--seed)))
        (message "Skill written: %s" file)
        (when (y-or-n-p "Open generated SKILL.md? ")
          (find-file file))
        file))))

(provide 'causal-self-walker-skill-gen)
;;; causal-self-walker-skill-gen.el ends here
