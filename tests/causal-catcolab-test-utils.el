;;; causal-catcolab-test-utils.el --- Test utilities for CatColab tests -*- lexical-binding: t; -*-

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

;; Shared fixtures and helpers for causal-catcolab ERT tests.

;;; Code:
(require 'ert)

;;; Fixtures

(defconst causal-catcolab-test--stub-complete
  '((refId . "abc-123")
    (name . "My Olog")
    (typeName . "simple-olog")
    (owner . ((username . "alice")))
    (createdAt . "2025-01-15T10:30:00Z"))
  "A fully-populated RefStub fixture.")

(defconst causal-catcolab-test--stub-minimal
  '((refId . "def-456")
    (name . "")
    (typeName . "petri-net"))
  "A minimal RefStub fixture with empty name and no owner.")

(defconst causal-catcolab-test--hyps-text
  "h1 : Nat
h2 : String
⊢ h1 = 0"
  "Sample *Goals* buffer text with two hypotheses and a goal.")

(defconst causal-catcolab-test--hyps-expected
  '(("h1" . "Nat") ("h2" . "String"))
  "Expected parse result from `causal-catcolab-test--hyps-text'.")

;;; Helpers

(defun causal-catcolab-test--alist-get-nested (keys alist)
  "Recursively look up KEYS path in nested ALIST."
  (if (null keys)
      alist
    (causal-catcolab-test--alist-get-nested
     (cdr keys)
     (alist-get (car keys) alist))))

(provide 'causal-catcolab-test-utils)
;;; causal-catcolab-test-utils.el ends here
