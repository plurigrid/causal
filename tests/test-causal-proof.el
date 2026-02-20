;;; test-causal-proof.el --- ERT tests for causal-proof -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'causal-proof-utils)
(require 'causal-proof)

;;; ── Unicode DB ───────────────────────────────────────────────────────

(ert-deftest test-causal-proof-unicode-db-all-keys ()
  "All unicode DB entries resolve to strings."
  (let ((keys '(:step-forward :step-backward :to-point :retract
                :goal :context :hole :tactic :check :lock
                :search :up-arrow :down-arrow :window :bridge
                :morphism :object :theory :elaborate :typecheck :compile)))
    (dolist (key keys)
      (should (stringp (causal-proof-unicode-get key))))))

;;; ── Backend Registry ─────────────────────────────────────────────────

(ert-deftest test-causal-proof-backends-non-empty ()
  "Built-in backends are registered."
  (should (> (length causal-proof-backends) 0)))

(ert-deftest test-causal-proof-backends-have-names ()
  "Every backend has a :name."
  (dolist (b causal-proof-backends)
    (should (stringp (plist-get b :name)))))

(ert-deftest test-causal-proof-backends-have-detect ()
  "Every backend has a :detect function."
  (dolist (b causal-proof-backends)
    (should (functionp (plist-get b :detect)))))

(ert-deftest test-causal-proof-builtin-backend-names ()
  "Expected built-in backends are present."
  (let ((names (mapcar (lambda (b) (plist-get b :name)) causal-proof-backends)))
    (should (member "Narya" names))
    (should (member "Proof General" names))
    (should (member "Lean" names))
    (should (member "OCaml" names))
    (should (member "DoubleTT" names))))

(ert-deftest test-causal-proof-register-replaces ()
  "`causal-proof-register-backend' replaces backend with same name."
  (let ((causal-proof-backends (copy-sequence causal-proof-backends))
        (test-b (list :name "TestOnly" :detect #'ignore)))
    (causal-proof-register-backend test-b)
    (let ((count (cl-count-if
                  (lambda (b) (string= "TestOnly" (plist-get b :name)))
                  causal-proof-backends)))
      (should (= 1 count)))
    ;; Register again, should still be 1
    (causal-proof-register-backend test-b)
    (should (= 1 (cl-count-if
                   (lambda (b) (string= "TestOnly" (plist-get b :name)))
                   causal-proof-backends)))))

;;; ── Detection in fundamental-mode ────────────────────────────────────

(ert-deftest test-causal-proof-no-backend-fundamental ()
  "No backend detected in fundamental-mode."
  (with-temp-buffer
    (fundamental-mode)
    (should (null (causal-proof--active-backend)))
    (should (string= "No backend" (causal-proof--backend-name)))))

;;; ── Dispatch fallback ────────────────────────────────────────────────

(ert-deftest test-causal-proof-dispatch-no-backend ()
  "Dispatch shows fallback message when no backend."
  (with-temp-buffer
    (fundamental-mode)
    (let ((msg nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (causal-proof--dispatch :step-fwd "test fallback"))
      (should (string= "test fallback" msg)))))

;;; ── Tactic insertion ─────────────────────────────────────────────────

(ert-deftest test-causal-proof-tactic-intro ()
  (with-temp-buffer (causal-proof-tactic-intro)
    (should (string= "intro " (buffer-string)))))

(ert-deftest test-causal-proof-tactic-apply ()
  (with-temp-buffer (causal-proof-tactic-apply)
    (should (string= "apply " (buffer-string)))))

(ert-deftest test-causal-proof-tactic-exact ()
  (with-temp-buffer (causal-proof-tactic-exact)
    (should (string= "exact " (buffer-string)))))

(ert-deftest test-causal-proof-tactic-simp ()
  (with-temp-buffer (causal-proof-tactic-simp)
    (should (string= "simp " (buffer-string)))))

(ert-deftest test-causal-proof-tactic-rfl ()
  (with-temp-buffer (causal-proof-tactic-rfl)
    (should (string= "rfl " (buffer-string)))))

(ert-deftest test-causal-proof-tactic-destruct ()
  (with-temp-buffer (causal-proof-tactic-destruct)
    (should (string= "destruct " (buffer-string)))))

(ert-deftest test-causal-proof-tactic-unfold ()
  (with-temp-buffer (causal-proof-tactic-unfold)
    (should (string= "unfold " (buffer-string)))))

;;; ── Hole navigation ──────────────────────────────────────────────────

(ert-deftest test-causal-proof-next-hole-question ()
  (with-temp-buffer
    (insert "lemma foo : ? := ?")
    (goto-char (point-min))
    (causal-proof-next-hole)
    (should (= (point) 14))))

(ert-deftest test-causal-proof-next-hole-sorry ()
  (with-temp-buffer
    (insert "theorem bar : Nat := sorry")
    (goto-char (point-min))
    (causal-proof-next-hole)
    (should (looking-back "sorry"))))

(ert-deftest test-causal-proof-prev-hole ()
  (with-temp-buffer
    (insert "lemma foo : ? := ?")
    (goto-char (point-max))
    (causal-proof-prev-hole)
    (should (looking-at "?"))))

;;; ── .dtt detection ───────────────────────────────────────────────────

(ert-deftest test-causal-proof-doublett-detect-by-extension ()
  "DoubleTT detected via .dtt file extension."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test.dtt")
    (fundamental-mode)
    (let ((b (causal-proof--active-backend)))
      (should b)
      (should (string= "DoubleTT" (plist-get b :name))))))

(ert-deftest test-causal-proof-settings-defaults ()
  (should (eq causal-proof-window-policy 'hybrid)))

(provide 'test-causal-proof)
;;; test-causal-proof.el ends here
