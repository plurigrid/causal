;;; test-causal-proofreader.el --- Causal ProofReader Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools

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

;;

;;; Code:

(require 'ert)
(require 'causal-proofreader-test-utils)
(require 'causal-proofreader)

;;; Utils Tests

(ert-deftest test-causal-proofreader-unicode-db ()
  "Test that all unicode DB entries resolve."
  (let ((keys '(:step-forward :step-backward :to-point :retract
                :goal :context :hole :tactic :check :lock
                :search :up-arrow :down-arrow :window :bridge)))
    (dolist (key keys)
      (should (stringp (causal-proofreader-unicode-get key))))))

(ert-deftest test-causal-proofreader-backend-detection ()
  "Test backend detection returns nil in fundamental-mode."
  (with-temp-buffer
    (fundamental-mode)
    (should (null (causal-proofreader--backend)))
    (should (string= "No backend" (causal-proofreader--backend-name)))))

(ert-deftest test-causal-proofreader-dispatch-no-backend ()
  "Test dispatch shows message when no backend available."
  (with-temp-buffer
    (fundamental-mode)
    (let ((msg nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (causal-proofreader--dispatch #'ignore #'ignore #'ignore "test fallback"))
      (should (string= "test fallback" msg)))))

;;; Tactic Insertion Tests

(ert-deftest test-causal-proofreader-tactic-intro ()
  "Test intro tactic insertion."
  (with-temp-buffer
    (causal-proofreader-tactic-intro)
    (should (string= "intro " (buffer-string)))))

(ert-deftest test-causal-proofreader-tactic-apply ()
  "Test apply tactic insertion."
  (with-temp-buffer
    (causal-proofreader-tactic-apply)
    (should (string= "apply " (buffer-string)))))

(ert-deftest test-causal-proofreader-tactic-exact ()
  "Test exact tactic insertion."
  (with-temp-buffer
    (causal-proofreader-tactic-exact)
    (should (string= "exact " (buffer-string)))))

(ert-deftest test-causal-proofreader-tactic-simp ()
  "Test simp tactic insertion."
  (with-temp-buffer
    (causal-proofreader-tactic-simp)
    (should (string= "simp " (buffer-string)))))

(ert-deftest test-causal-proofreader-tactic-rfl ()
  "Test rfl tactic insertion."
  (with-temp-buffer
    (causal-proofreader-tactic-rfl)
    (should (string= "rfl " (buffer-string)))))

;;; Hole Navigation Tests

(ert-deftest test-causal-proofreader-next-hole ()
  "Test next-hole navigation."
  (with-temp-buffer
    (insert "lemma foo : ? := ?")
    (goto-char (point-min))
    (causal-proofreader-next-hole)
    (should (= (point) 14))))

(ert-deftest test-causal-proofreader-prev-hole ()
  "Test prev-hole navigation."
  (with-temp-buffer
    (insert "lemma foo : ? := ?")
    (goto-char (point-max))
    (causal-proofreader-prev-hole)
    (should (= (point) 18))))

;;; Remaining Tactic Insertion Tests

(ert-deftest test-causal-proofreader-tactic-rewrite ()
  "Test rewrite tactic insertion."
  (with-temp-buffer
    (causal-proofreader-tactic-rewrite)
    (should (string= "rewrite " (buffer-string)))))

(ert-deftest test-causal-proofreader-tactic-ring ()
  "Test ring tactic insertion."
  (with-temp-buffer
    (causal-proofreader-tactic-ring)
    (should (string= "ring " (buffer-string)))))

(ert-deftest test-causal-proofreader-tactic-omega ()
  "Test omega tactic insertion."
  (with-temp-buffer
    (causal-proofreader-tactic-omega)
    (should (string= "omega " (buffer-string)))))

(ert-deftest test-causal-proofreader-tactic-cases ()
  "Test cases tactic insertion."
  (with-temp-buffer
    (causal-proofreader-tactic-cases)
    (should (string= "cases " (buffer-string)))))

(ert-deftest test-causal-proofreader-tactic-induction ()
  "Test induction tactic insertion."
  (with-temp-buffer
    (causal-proofreader-tactic-induction)
    (should (string= "induction " (buffer-string)))))

;;; Insert-tactic helper

(ert-deftest test-causal-proofreader-insert-tactic-appends-space ()
  "insert-tactic always appends a trailing space."
  (with-temp-buffer
    (causal-proofreader--insert-tactic "custom_tactic")
    (should (string= "custom_tactic " (buffer-string)))))

(ert-deftest test-causal-proofreader-insert-tactic-at-point ()
  "insert-tactic inserts at point, not at end."
  (with-temp-buffer
    (insert "prefix ")
    (causal-proofreader--insert-tactic "apply")
    (should (string= "prefix apply " (buffer-string)))))

;;; Backend Detection Tests

(ert-deftest test-causal-proofreader-backend-name-completeness ()
  "backend-name returns a string for every backend symbol."
  (dolist (sym '(narya doublett lean proof-general nil))
    (cl-letf (((symbol-function 'causal-proofreader--backend)
               (lambda () sym)))
      (should (stringp (causal-proofreader--backend-name))))))

(ert-deftest test-causal-proofreader-backend-nil-in-fundamental ()
  "backend returns nil in fundamental-mode (no proof assistant)."
  (with-temp-buffer
    (fundamental-mode)
    (should (null (causal-proofreader--backend)))))

(ert-deftest test-causal-proofreader-proof-general-p-nil ()
  "proof-general-p returns nil when proof-site not loaded."
  (with-temp-buffer
    (should (null (causal-proofreader--proof-general-p)))))

(ert-deftest test-causal-proofreader-lean-mode-p-nil ()
  "lean-mode-p returns nil in fundamental-mode."
  (with-temp-buffer
    (fundamental-mode)
    (should (null (causal-proofreader--lean-mode-p)))))

(ert-deftest test-causal-proofreader-narya-mode-p-nil ()
  "narya-mode-p returns nil in fundamental-mode."
  (with-temp-buffer
    (fundamental-mode)
    (should (null (causal-proofreader--narya-mode-p)))))

(ert-deftest test-causal-proofreader-doublett-mode-p-by-extension ()
  "doublett-mode-p detects .dtt file extension."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test.dtt")
    (should (causal-proofreader--doublett-mode-p))))

(ert-deftest test-causal-proofreader-doublett-mode-p-non-dtt ()
  "doublett-mode-p returns nil for non-.dtt files."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test.v")
    (should (null (causal-proofreader--doublett-mode-p)))))

;;; Dispatch Tests

(ert-deftest test-causal-proofreader-dispatch-calls-pg ()
  "dispatch calls PG function when backend is proof-general."
  (let ((called nil))
    (cl-letf (((symbol-function 'causal-proofreader--backend)
               (lambda () 'proof-general)))
      (causal-proofreader--dispatch
       (lambda () (interactive) (setq called 'pg))
       (lambda () (interactive) (setq called 'lean))
       (lambda () (interactive) (setq called 'narya)))
      (should (eq 'pg called)))))

(ert-deftest test-causal-proofreader-dispatch-calls-lean ()
  "dispatch calls Lean function when backend is lean."
  (let ((called nil))
    (cl-letf (((symbol-function 'causal-proofreader--backend)
               (lambda () 'lean)))
      (causal-proofreader--dispatch
       (lambda () (interactive) (setq called 'pg))
       (lambda () (interactive) (setq called 'lean))
       (lambda () (interactive) (setq called 'narya)))
      (should (eq 'lean called)))))

(ert-deftest test-causal-proofreader-dispatch-calls-narya ()
  "dispatch calls Narya function when backend is narya."
  (let ((called nil))
    (cl-letf (((symbol-function 'causal-proofreader--backend)
               (lambda () 'narya)))
      (causal-proofreader--dispatch
       (lambda () (interactive) (setq called 'pg))
       (lambda () (interactive) (setq called 'lean))
       (lambda () (interactive) (setq called 'narya)))
      (should (eq 'narya called)))))

(ert-deftest test-causal-proofreader-dispatch-doublett-falls-to-narya ()
  "dispatch falls back to narya-fn for doublett when no doublett-fn."
  (let ((called nil))
    (cl-letf (((symbol-function 'causal-proofreader--backend)
               (lambda () 'doublett)))
      (causal-proofreader--dispatch
       (lambda () (interactive) (setq called 'pg))
       (lambda () (interactive) (setq called 'lean))
       (lambda () (interactive) (setq called 'narya)))
      (should (eq 'narya called)))))

(ert-deftest test-causal-proofreader-dispatch-doublett-with-doublett-fn ()
  "dispatch calls doublett-fn when provided and backend is doublett."
  (let ((called nil))
    (cl-letf (((symbol-function 'causal-proofreader--backend)
               (lambda () 'doublett)))
      (causal-proofreader--dispatch
       (lambda () (interactive) (setq called 'pg))
       (lambda () (interactive) (setq called 'lean))
       (lambda () (interactive) (setq called 'narya))
       nil
       (lambda () (interactive) (setq called 'doublett)))
      (should (eq 'doublett called)))))

(ert-deftest test-causal-proofreader-dispatch-custom-fallback-msg ()
  "dispatch shows custom fallback message when no backend."
  (with-temp-buffer
    (fundamental-mode)
    (let ((msg nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (causal-proofreader--dispatch #'ignore #'ignore #'ignore "Custom fallback"))
      (should (string= "Custom fallback" msg)))))

;;; Hole Navigation Edge Cases

(ert-deftest test-causal-proofreader-next-hole-no-holes ()
  "next-hole returns nil when no ? found."
  (with-temp-buffer
    (insert "lemma foo : Nat := 42")
    (goto-char (point-min))
    (should (null (causal-proofreader-next-hole)))))

(ert-deftest test-causal-proofreader-prev-hole-no-holes ()
  "prev-hole returns nil when no ? found."
  (with-temp-buffer
    (insert "lemma foo : Nat := 42")
    (goto-char (point-max))
    (should (null (causal-proofreader-prev-hole)))))

(ert-deftest test-causal-proofreader-next-hole-multiple ()
  "next-hole advances through multiple holes in sequence."
  (with-temp-buffer
    (insert "? + ? = ?")
    (goto-char (point-min))
    (causal-proofreader-next-hole)
    (should (= (point) 2))
    (causal-proofreader-next-hole)
    (should (= (point) 6))
    (causal-proofreader-next-hole)
    (should (= (point) 10))))

;;; Mnemosyne Integration Tests

(ert-deftest test-causal-proofreader-save-to-mnemosyne-delegates ()
  "save-to-mnemosyne delegates to sophia-mnemosyne-save-buffer."
  (let ((called nil))
    (cl-letf (((symbol-function 'sophia-mnemosyne-save-buffer)
               (lambda () (setq called t))))
      (causal-proofreader-save-to-mnemosyne)
      (should called))))

(ert-deftest test-causal-proofreader-save-to-mnemosyne-errors-without ()
  "save-to-mnemosyne errors when sophia-mnemosyne not loaded."
  (cl-letf (((symbol-function 'fboundp) (lambda (s) nil)))
    (should-error (causal-proofreader-save-to-mnemosyne) :type 'user-error)))

(ert-deftest test-causal-proofreader-wire-proof-dep-delegates ()
  "wire-proof-dep delegates to sophia-mnemosyne-create-wire."
  (let ((wire-args nil))
    (cl-letf (((symbol-function 'sophia-mnemosyne-create-wire)
               (lambda (from to pred)
                 (setq wire-args (list from to pred)))))
      (causal-proofreader-wire-proof-dep "lemma-1" "thm-2")
      (should (equal '("lemma-1" "thm-2" "proof:dependsOn") wire-args)))))

;;; Unicode Database Completeness

(ert-deftest test-causal-proofreader-unicode-db-all-keys ()
  "Unicode DB contains all keys used by transient menus."
  (let ((keys '(:step-forward :step-backward :to-point :retract
                :goal :context :hole :tactic :check :lock
                :search :up-arrow :down-arrow :window :bridge
                :morphism :object :theory :elaborate :geb)))
    (dolist (key keys)
      (should (stringp (causal-proofreader-unicode-get key))))))

(ert-deftest test-causal-proofreader-unicode-db-count ()
  "Unicode DB has exactly 20 entries."
  (should (= 20 (length causal-proofreader-unicode-db))))

;;; Settings Tests

(ert-deftest test-causal-proofreader-settings-defaults ()
  "Test default settings values."
  (should (eq causal-proofreader-window-policy 'hybrid))
  (should (eq causal-proofreader-auto-detect-backend t)))

(ert-deftest test-causal-proofreader-customization-group ()
  "causal-proofreader customization group exists."
  (should (get 'causal-proofreader 'custom-group)))

(ert-deftest test-causal-proofreader-about-info-docstring ()
  "about-info has a docstring mentioning Proof General."
  (should (string-match-p "Proof General"
                          (documentation 'causal-proofreader-about-info))))

(provide 'test-causal-proofreader)
;;; test-causal-proofreader.el ends here
