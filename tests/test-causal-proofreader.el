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

;;; Settings Tests

(ert-deftest test-causal-proofreader-settings-defaults ()
  "Test default settings values."
  (should (eq causal-proofreader-window-policy 'hybrid))
  (should (eq causal-proofreader-auto-detect-backend t)))

(provide 'test-causal-proofreader)
;;; test-causal-proofreader.el ends here
