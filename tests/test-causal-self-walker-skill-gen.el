;;; test-causal-self-walker-skill-gen.el --- ERT tests for ASI skill generation -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools

;;; Commentary:

;; Offline ERT tests for causal-self-walker-skill-gen.el.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'causal-self-walker)
(unless (featurep 'causal-self-walker-mc)
  (require 'causal-self-walker-mc))
(unless (featurep 'causal-self-walker-skill-gen)
  (require 'causal-self-walker-skill-gen))

;;; ── Feature loading ──────────────────────────────────────────────────────

(ert-deftest test-skill-gen-loads ()
  "causal-self-walker-skill-gen feature is available."
  (should (featurep 'causal-self-walker-skill-gen)))

;;; ── Slugify ──────────────────────────────────────────────────────────────

(ert-deftest test-skill-gen-slugify-basic ()
  "Basic string is slugified to kebab-case."
  (should (string= "hello-world"
                    (causal-self-walker-skill-gen--slugify "Hello World"))))

(ert-deftest test-skill-gen-slugify-special-chars ()
  "Special characters become hyphens."
  (should (string= "n-0-n"
                    (causal-self-walker-skill-gen--slugify "n + 0 = n"))))

(ert-deftest test-skill-gen-slugify-unicode ()
  "Unicode symbols are stripped."
  (should (string= "x-x"
                    (causal-self-walker-skill-gen--slugify "∀ x, x"))))

(ert-deftest test-skill-gen-slugify-truncates ()
  "Long strings are truncated to 60 chars."
  (let ((long (make-string 100 ?a)))
    (should (<= (length (causal-self-walker-skill-gen--slugify long)) 60))))

(ert-deftest test-skill-gen-slugify-no-leading-trailing-hyphens ()
  "No leading or trailing hyphens."
  (let ((result (causal-self-walker-skill-gen--slugify "  --hello-- ")))
    (should-not (string-prefix-p "-" result))
    (should-not (string-suffix-p "-" result))))

;;; ── Tactic signature ─────────────────────────────────────────────────────

(ert-deftest test-skill-gen-tactic-signature ()
  "Tactic signature extracts non-empty tactics joined by hyphens."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 0))
    (causal-self-walker--record-state 0 nil "g" "" "" 'initial)
    (causal-self-walker--record-state 1 nil "g2" "intro" "fn" 'active)
    (causal-self-walker--record-state 2 nil "" "simp" "fn" 'qed)
    (let* ((chain (causal-self-walker--chain-ordered))
           (sig (causal-self-walker-skill-gen--tactic-signature chain)))
      (should (string= "intro-simp" sig)))))

(ert-deftest test-skill-gen-tactic-signature-empty-chain ()
  "Empty chain produces empty signature."
  (should (string= "" (causal-self-walker-skill-gen--tactic-signature nil))))

;;; ── Skill name ───────────────────────────────────────────────────────────

(ert-deftest test-skill-gen-name-from-goal ()
  "Skill name includes slugified goal and step count."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 0))
    (causal-self-walker--record-state 0 nil "n + 0 = n" "" "" 'initial)
    (causal-self-walker--record-state 1 nil "" "simp" "fn" 'qed)
    (let* ((chain (causal-self-walker--chain-ordered))
           (name (causal-self-walker-skill-gen--skill-name chain)))
      (should (string-match-p "n-0-n" name))
      (should (string-match-p "1-step" name)))))

(ert-deftest test-skill-gen-name-fallback ()
  "Fallback name when no goal available."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 0))
    (causal-self-walker--record-state 0 nil "" "" "" 'initial)
    (let* ((chain (causal-self-walker--chain-ordered))
           (name (causal-self-walker-skill-gen--skill-name chain)))
      (should (string-match-p "proof" name)))))

;;; ── Walk trit ────────────────────────────────────────────────────────────

(ert-deftest test-skill-gen-walk-trit-balanced ()
  "Balanced walk (intro+simp) produces trit 0."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 0))
    (causal-self-walker--record-state 0 nil "g" "" "" 'initial)
    (causal-self-walker--record-state 1 nil "g2" "intro" "fn" 'active)
    (causal-self-walker--record-state 2 nil "" "simp" "fn" 'qed)
    (let* ((chain (causal-self-walker--chain-ordered))
           (trit (causal-self-walker-skill-gen--walk-trit chain)))
      (should (= 0 trit)))))

(ert-deftest test-skill-gen-walk-trit-generative ()
  "Generative-heavy walk (intro+cases) produces trit != 0."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 0))
    (causal-self-walker--record-state 0 nil "g" "" "" 'initial)
    (causal-self-walker--record-state 1 nil "g2" "intro" "fn" 'active)
    (causal-self-walker--record-state 2 nil "" "cases" "fn" 'qed)
    (let* ((chain (causal-self-walker--chain-ordered))
           (trit (causal-self-walker-skill-gen--walk-trit chain)))
      (should (member trit '(-1 0 1))))))

(ert-deftest test-skill-gen-trit-label ()
  "Trit labels are correct."
  (should (string-match-p "generative"
                          (causal-self-walker-skill-gen--trit-label 1)))
  (should (string-match-p "reductive"
                          (causal-self-walker-skill-gen--trit-label -1)))
  (should (string-match-p "balanced"
                          (causal-self-walker-skill-gen--trit-label 0))))

;;; ── SKILL.md generation ──────────────────────────────────────────────────

(ert-deftest test-skill-gen-format-has-frontmatter ()
  "Generated SKILL.md starts with YAML frontmatter."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 42))
    (causal-self-walker--record-state 0 '(("h" . "Nat")) "h = 0" "" "" 'initial)
    (causal-self-walker--record-state 1 nil "No Goals" "simp" "fn" 'qed)
    (let* ((chain (causal-self-walker--chain-ordered))
           (md (causal-self-walker-skill-gen--format-skill-md chain 42)))
      (should (string-prefix-p "---\n" md))
      (should (string-match-p "name:" md))
      (should (string-match-p "trit:" md))
      (should (string-match-p "seed:" md)))))

(ert-deftest test-skill-gen-format-has-tactic-table ()
  "Generated SKILL.md includes a tactic sequence table."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 42))
    (causal-self-walker--record-state 0 nil "n + 0 = n" "" "" 'initial)
    (causal-self-walker--record-state 1 nil "No Goals" "simp" "fn" 'qed)
    (let* ((chain (causal-self-walker--chain-ordered))
           (md (causal-self-walker-skill-gen--format-skill-md chain 42)))
      (should (string-match-p "Tactic Sequence" md))
      (should (string-match-p "`simp`" md)))))

(ert-deftest test-skill-gen-format-has-state-chain ()
  "Generated SKILL.md includes state chain block."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 42))
    (causal-self-walker--record-state 0 nil "g" "" "" 'initial)
    (causal-self-walker--record-state 1 nil "" "intro" "fn" 'qed)
    (let* ((chain (causal-self-walker--chain-ordered))
           (md (causal-self-walker-skill-gen--format-skill-md chain 42)))
      (should (string-match-p "State Chain" md))
      (should (string-match-p "State 0:" md)))))

(ert-deftest test-skill-gen-format-has-gf3-triad ()
  "Generated SKILL.md includes GF(3) triad section."
  (let ((causal-self-walker--chain nil)
        (causal-self-walker--seed 42))
    (causal-self-walker--record-state 0 nil "g" "" "" 'initial)
    (causal-self-walker--record-state 1 nil "" "intro" "fn" 'qed)
    (let* ((chain (causal-self-walker--chain-ordered))
           (md (causal-self-walker-skill-gen--format-skill-md chain 42)))
      (should (string-match-p "GF(3) Triad" md))
      (should (string-match-p "self-walker" md)))))

;;; ── File output ──────────────────────────────────────────────────────────

(ert-deftest test-skill-gen-write-creates-file ()
  "write-skill creates a SKILL.md file in a named directory."
  (let* ((tmpdir (make-temp-file "skill-gen-test" t))
         (causal-self-walker-skill-gen-output-dir tmpdir)
         (causal-self-walker--chain nil)
         (causal-self-walker--seed 42))
    (unwind-protect
        (progn
          (causal-self-walker--record-state 0 nil "x = x" "" "" 'initial)
          (causal-self-walker--record-state 1 nil "No Goals" "rfl" "fn" 'qed)
          (let* ((chain (causal-self-walker--chain-ordered))
                 (file (causal-self-walker-skill-gen--write-skill chain 42)))
            (should (file-exists-p file))
            (should (string-match-p "SKILL.md" file))
            (with-temp-buffer
              (insert-file-contents file)
              (should (string-match-p "name:" (buffer-string))))))
      (delete-directory tmpdir t))))

(ert-deftest test-skill-gen-output-dir-default ()
  "Default output dir points to ~/i/asi/skills/."
  (let ((causal-self-walker-skill-gen-output-dir nil))
    (should (string-match-p "asi/skills"
                            (causal-self-walker-skill-gen--output-dir)))))

(provide 'test-causal-self-walker-skill-gen)
;;; test-causal-self-walker-skill-gen.el ends here
