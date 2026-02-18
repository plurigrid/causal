;;; test-causal-catcolab.el --- ERT tests for causal-catcolab -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, categories

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

;; Offline ERT tests for causal-catcolab-utils.el and causal-catcolab.el.
;; No network calls are made; all tests use fixtures and unit-testable
;; pure functions.

;;; Code:
(require 'ert)
(require 'causal-catcolab-test-utils)
(require 'causal-catcolab-utils)

;;; ── Defaults ─────────────────────────────────────────────────────────────────

(ert-deftest test-causal-catcolab-default-url ()
  "Default backend URL points to backend.catcolab.org."
  (should (string-equal "https://backend.catcolab.org"
                        causal-catcolab-url)))

(ert-deftest test-causal-catcolab-default-token-nil ()
  "Default auth token is nil (anonymous access)."
  (should (null causal-catcolab-token)))

;;; ── URL helpers ──────────────────────────────────────────────────────────────

(ert-deftest test-causal-catcolab-url-for-ref ()
  "url-for-ref produces catcolab.org app URL."
  (let ((ref-id "abc-123-def-456"))
    (should (string-equal
             "https://catcolab.org/app/new/abc-123-def-456"
             (causal-catcolab-url-for-ref ref-id)))))

(ert-deftest test-causal-catcolab-url-for-ref-uuid-format ()
  "url-for-ref works with realistic UUID strings."
  (let ((uuid "550e8400-e29b-41d4-a716-446655440000"))
    (should (string-prefix-p "https://catcolab.org/app/new/"
                             (causal-catcolab-url-for-ref uuid)))
    (should (string-suffix-p uuid (causal-catcolab-url-for-ref uuid)))))

;;; ── Model constructors ───────────────────────────────────────────────────────

(ert-deftest test-causal-catcolab-new-model-type ()
  "`causal-catcolab--new-model' sets type to \"model\"."
  (let ((m (causal-catcolab--new-model "simple-olog" "Test")))
    (should (string-equal "model" (alist-get "type" m nil nil #'string-equal)))))

(ert-deftest test-causal-catcolab-new-model-name ()
  "`causal-catcolab--new-model' stores the given name."
  (let ((m (causal-catcolab--new-model "petri-net" "My Net")))
    (should (string-equal "My Net" (alist-get "name" m nil nil #'string-equal)))))

(ert-deftest test-causal-catcolab-new-model-theory ()
  "`causal-catcolab--new-model' stores the theory ID."
  (let ((m (causal-catcolab--new-model "causal-loop" "Loop")))
    (should (string-equal "causal-loop"
                          (alist-get "theory" m nil nil #'string-equal)))))

(ert-deftest test-causal-catcolab-new-model-notebook ()
  "`causal-catcolab--new-model' includes an empty Notebook cell list."
  (let* ((m (causal-catcolab--new-model "simple-olog" "Test"))
         (nb (alist-get "notebook" m nil nil #'string-equal)))
    (should (listp nb))
    (should (string-equal "Notebook" (alist-get "tag" nb nil nil #'string-equal)))))

;;; ── Olog-from-proof ──────────────────────────────────────────────────────────

(ert-deftest test-causal-catcolab-olog-from-proof-type ()
  "`causal-catcolab--olog-from-proof' sets type to \"model\"."
  (let ((doc (causal-catcolab--olog-from-proof
              "Test Olog"
              '(("h1" . "Nat"))
              "h1 = 0")))
    (should (string-equal "model" (alist-get "type" doc nil nil #'string-equal)))))

(ert-deftest test-causal-catcolab-olog-from-proof-theory ()
  "`causal-catcolab--olog-from-proof' uses simple-olog theory."
  (let ((doc (causal-catcolab--olog-from-proof "T" '() "goal")))
    (should (string-equal "simple-olog"
                          (alist-get "theory" doc nil nil #'string-equal)))))

(ert-deftest test-causal-catcolab-olog-from-proof-includes-goal-object ()
  "`causal-catcolab--olog-from-proof' adds a Goal object."
  (let* ((doc (causal-catcolab--olog-from-proof "T" '() "x : Nat"))
         (content (alist-get "content" doc nil nil #'string-equal))
         (obs (alist-get "ob_generators" content nil nil #'string-equal)))
    (should (vectorp obs))
    (should (> (length obs) 0))
    ;; The last element should be the Goal object
    (let ((last-ob (aref obs (1- (length obs)))))
      (should (string-equal "goal"
                            (alist-get "id" last-ob nil nil #'string-equal))))))

(ert-deftest test-causal-catcolab-olog-from-proof-hyp-count ()
  "`causal-catcolab--olog-from-proof' creates N+1 objects (hyps + goal)."
  (let* ((hyps '(("h1" . "Nat") ("h2" . "String") ("h3" . "Prop")))
         (doc (causal-catcolab--olog-from-proof "T" hyps "h1 + h2 = h3"))
         (content (alist-get "content" doc nil nil #'string-equal))
         (obs (alist-get "ob_generators" content nil nil #'string-equal)))
    ;; 3 hyps + 1 goal = 4
    (should (= 4 (length obs)))))

(ert-deftest test-causal-catcolab-olog-from-proof-empty-hyps ()
  "`causal-catcolab--olog-from-proof' with no hyps produces 1 object (goal only)."
  (let* ((doc (causal-catcolab--olog-from-proof "T" '() "True"))
         (content (alist-get "content" doc nil nil #'string-equal))
         (obs (alist-get "ob_generators" content nil nil #'string-equal)))
    (should (= 1 (length obs)))))

;;; ── Parse hypotheses ─────────────────────────────────────────────────────────

(ert-deftest test-causal-catcolab-parse-hypotheses-basic ()
  "`causal-catcolab--parse-hypotheses' extracts name:type pairs."
  (let ((result (causal-catcolab--parse-hypotheses
                 causal-catcolab-test--hyps-text)))
    (should (equal 2 (length result)))
    (should (equal "Nat" (cdr (assoc "h1" result))))
    (should (equal "String" (cdr (assoc "h2" result))))))

(ert-deftest test-causal-catcolab-parse-hypotheses-ignores-goal-line ()
  "`causal-catcolab--parse-hypotheses' does not parse ⊢ lines."
  (let ((result (causal-catcolab--parse-hypotheses
                 "h : Nat\n⊢ h = 0\n")))
    (should (= 1 (length result)))
    (should (string-equal "Nat" (cdr (assoc "h" result))))))

(ert-deftest test-causal-catcolab-parse-hypotheses-empty ()
  "`causal-catcolab--parse-hypotheses' returns nil on empty text."
  (let ((result (causal-catcolab--parse-hypotheses "")))
    (should (null result))))

(ert-deftest test-causal-catcolab-parse-hypotheses-lean-style ()
  "`causal-catcolab--parse-hypotheses' handles Lean 4 style goals buffer."
  (let* ((text "  n : ℕ\n  ih : n = 0\n⊢ n.succ ≠ 0\n")
         (result (causal-catcolab--parse-hypotheses text)))
    (should (= 2 (length result)))
    (should (assoc "n" result))
    (should (assoc "ih" result))))

(ert-deftest test-causal-catcolab-parse-hypotheses-skips-reserved ()
  "`causal-catcolab--parse-hypotheses' skips 'case', 'α', 'β'."
  (let* ((text "case : Nat\nα : Type\nβ : Prop\nh : Bool\n")
         (result (causal-catcolab--parse-hypotheses text)))
    ;; 'case', 'α', 'β' are in the exclusion list; only 'h' should remain
    (should (= 1 (length result)))
    (should (assoc "h" result))))

;;; ── RPC ID counter ───────────────────────────────────────────────────────────

(ert-deftest test-causal-catcolab-rpc-id-increments ()
  "`causal-catcolab--next-id' returns strictly increasing IDs."
  (let ((id1 (causal-catcolab--next-id))
        (id2 (causal-catcolab--next-id))
        (id3 (causal-catcolab--next-id)))
    (should (< id1 id2))
    (should (< id2 id3))))

;;; ── Stub display formatting ──────────────────────────────────────────────────

(ert-deftest test-causal-catcolab-stub-line-complete ()
  "`causal-catcolab--stub-line' formats a complete stub as a string."
  (let ((line (causal-catcolab--stub-line causal-catcolab-test--stub-complete)))
    (should (stringp line))
    (should (string-match-p "My Olog" line))
    (should (string-match-p "abc-123" line))
    (should (string-match-p "alice" line))
    (should (string-match-p "simple-olog" line))))

(ert-deftest test-causal-catcolab-stub-line-unnamed ()
  "`causal-catcolab--stub-line' uses \"(unnamed)\" when name is empty."
  (let ((line (causal-catcolab--stub-line causal-catcolab-test--stub-minimal)))
    (should (string-match-p "(unnamed)" line))))

(ert-deftest test-causal-catcolab-stub-line-no-owner ()
  "`causal-catcolab--stub-line' shows \"public\" when no owner."
  (let ((line (causal-catcolab--stub-line causal-catcolab-test--stub-minimal)))
    (should (string-match-p "public" line))))

(ert-deftest test-causal-catcolab-stub-line-date-truncated ()
  "`causal-catcolab--stub-line' shows only the date portion (10 chars)."
  (let ((line (causal-catcolab--stub-line causal-catcolab-test--stub-complete)))
    ;; Should show "2025-01-15", not the full timestamp
    (should (string-match-p "2025-01-15" line))
    (should (not (string-match-p "T10:30:00Z" line)))))

;;; ── Status label ─────────────────────────────────────────────────────────────

(ert-deftest test-causal-catcolab-status-label-anon ()
  "`causal-catcolab--status-label' shows [anon] when no token."
  (let ((causal-catcolab-url "https://backend.catcolab.org")
        (causal-catcolab-token nil))
    (should (string-match-p "\\[anon\\]"
                            (causal-catcolab--status-label)))))

(ert-deftest test-causal-catcolab-status-label-auth ()
  "`causal-catcolab--status-label' shows [auth] when token is set."
  (let ((causal-catcolab-url "https://backend.catcolab.org")
        (causal-catcolab-token "fake-firebase-token"))
    (should (string-match-p "\\[auth\\]"
                            (causal-catcolab--status-label)))))

(ert-deftest test-causal-catcolab-status-label-includes-url ()
  "`causal-catcolab--status-label' includes the backend URL."
  (let ((causal-catcolab-url "http://localhost:8000")
        (causal-catcolab-token nil))
    (should (string-match-p "localhost:8000"
                            (causal-catcolab--status-label)))))

(provide 'test-causal-catcolab)
;;; test-causal-catcolab.el ends here
