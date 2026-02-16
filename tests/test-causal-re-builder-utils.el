;;; test-causal-re-builder-utils.el --- Causal Re-Builder Utils Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
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
(require 'causal-re-builder-test-utils)
(require 'causal-re-builder-utils)

(ert-deftest test-causal-re-builder-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal (causal-re-builder-unicode-get :previous) "Previous"))
    (should (string-equal (causal-re-builder-unicode-get :next) "Next")))

  (let ((causal-lib-use-unicode t))
    (should (string-equal (causal-re-builder-unicode-get :previous) "↑"))
    (should (string-equal (causal-re-builder-unicode-get :next) "↓"))))

;;; Interactive test vectors

;; Unfortunately, Elisp does not support raw strings, making it problematic to
;; write a unit test that actually works with a string intended to be entered
;; interactively as the escape character is processed (e.g. '\').

;; In lieu of that are test strings that are intended to be entered directly
;; into `re-builder' for manual testing of
;; `causal-re-builder--re-elisp-to-interactive' and
;; `causal-re-builder--re-elisp-to-grep-interactive'.

;; Test:
;; ("m" "This is something" ayo
;; ("w" "Interactive" causal-re-builder-copy
;; "(\(\"[[:alpha:]-]*\"\)[[:blank:]]*\(\"[[:graph:][:blank:]]*\"\)[[:blank:]]*\([[:graph:]]*\)"

;; 03:29:18
;; 14:29:24
;; "\([[:digit:]]\{2\}\):\([[:digit:]]\{2\}\):\([[:digit:]]\{2\}\)"


;; kickingvegas@gmail.com
;; cyj9h@virginia.edu
;; hallo@bingsu.org
;; "\([[:alpha:]]*@[[:alpha:]\.]*.\(com\|org\|edu\)\)"

;;!!!: Abandoned test due to lack of raw string support.
;; (ert-deftest test-causal-re-builder--re-elisp-to-interactive ()
;;   (let ((test-vectors (list
;;                         (list
;;                          "(\\(\\\"[[:alpha:]-]*\\\"\\)[[:blank:]]*\\(\\\"[[:graph:][:blank:]]*\\\"\\)[[:blank:]]*\\([[:graph:]]*\\)"
;;                          "((\"[[:alpha:]-]*\")[[:blank:]]*(\"[[:graph:][:blank:]]*\")[[:blank:]]*([[:graph:]]*)")
;;                        )))

;;     (mapc (lambda (x)
;;             (let ((test (causal-re-builder--re-elisp-to-interactive (nth 0 x)))
;;                   (control (nth 1 x)))
;;               (should (string-equal test control))))
;;           test-vectors)))

(provide 'test-causal-re-builder-utils)
;;; test-causal-re-builder-utils.el ends here
