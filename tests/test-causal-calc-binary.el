;;; test-causal-calc-binary.el --- Causal Binary Menu Tests  -*- lexical-binding: t; -*-

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
(require 'causal-calc-test-utils)
(require 'causal-calc-binary)

(ert-deftest test-causal-calc-binary-tmenu-bindings ()
  (causalt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-group-char) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-leading-zeros) (lambda (x) (interactive)(print "WARNING: override"))))
    (let ((test-vectors '(("&" . causal-calc--and)
                        ("|" . causal-calc--or)
                        ("^" . causal-calc--xor)
                        ("d" . causal-calc--diff)
                        ("!" . causal-calc--not)

                        ("l" . causal-calc--lshift-binary)
                        ("r" . causal-calc--rshift-binary)

                        ("ì" . causal-calc--lshift-arith)
                        ("ò" . causal-calc--rshift-arith)
                        ("" . causal-calc--rotate-binary)

                        ("z" . calc-leading-zeros)
                        ("," . calc-group-char)
                        ("R" . causal-calc-radix-tmenu)
                        ("w" . causal-calc--word-size)
                        ("u" . causal-calc--unpack-bits)
                        ("p" . causal-calc--pack-bits))))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-binary-tmenu
                                       '(lambda () (random 5000)))))

  (causalt-calc-breakdown t t))


(ert-deftest test-causal-calc--and ()
  (causalt-calc-setup)
  (calc-push 11)
  (calc-push 12)
  (call-interactively #'causal-calc--and)
  (should (= (calc-top) 8))

  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--or ()
  (causalt-calc-setup)
  (calc-push 11)
  (calc-push 12)
  (call-interactively #'causal-calc--or)
  (should (= (calc-top) 15))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--xor ()
  (causalt-calc-setup)
  (calc-push 11)
  (calc-push 12)
  (call-interactively #'causal-calc--xor)
  (should (= (calc-top) 7))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--diff ()
  (causalt-calc-setup)
  (calc-push 11)
  (calc-push 12)
  (call-interactively #'causal-calc--diff)
  (should (= (calc-top) 3))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--not ()
  (causalt-calc-setup)
  (calc-word-size 8)
  (calc-push 11)
  (call-interactively #'causal-calc--not)
  (should (= (calc-top) 244))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--lshift-binary ()
  (causalt-calc-setup)
  (calc-word-size 8)
  (calc-push 12)
  (call-interactively #'causal-calc--lshift-binary)
  (should (= (calc-top) 24))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--rshift-binary ()
  (causalt-calc-setup)
  (calc-word-size 8)
  (calc-push 24)
  (call-interactively #'causal-calc--rshift-binary)
  (should (= (calc-top) 12))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--lshift-arith ()
  (causalt-calc-setup)
  (calc-word-size 8)
  (calc-push 12)
  (call-interactively #'causal-calc--lshift-arith)
  (should (= (calc-top) 24))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--rshift-arith ()
  (causalt-calc-setup)
  (calc-word-size 8)
  (calc-push 24)
  (call-interactively #'causal-calc--rshift-arith)
  (should (= (calc-top) 12))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--rotate-binary ()
  (causalt-calc-setup)
  (calc-word-size 8)
  (calc-push 24)
  (call-interactively #'causal-calc--rotate-binary)
  (should (= (calc-top) 48))
  (causalt-calc-breakdown t))

;; (ert-deftest test-causal-calc--word-size ()
;;   (causalt-calc-setup)
;;   (causalt-calc-breakdown t))

;; (ert-deftest test-causal-calc--unpack-bits ()
;;   (causalt-calc-setup)
;;   (causalt-calc-breakdown t))

;; (ert-deftest test-causal-calc--pack-bits ()
;;   (causalt-calc-setup)
;;   (causalt-calc-breakdown t))

(provide 'test-causal-calc-binary)
;;; test-causal-calc-binary.el ends here
