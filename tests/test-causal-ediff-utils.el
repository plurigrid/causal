;;; test-causal-ediff-utils.el --- Causal Make Utils Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Y. Choi

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
(require 'causal-ediff-test-utils)
(require 'causal-ediff-utils)

(ert-deftest test-causal-ediff-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal (causal-ediff-unicode-get :previous) "Previous"))
    (should (string-equal (causal-ediff-unicode-get :next) "Next"))
    (should (string-equal (causal-ediff-unicode-get :scroll-to-right) "Scroll to right"))
    (should (string-equal (causal-ediff-unicode-get :scroll-to-left) "Scroll to left"))
    (should (string-equal (causal-ediff-unicode-get :refresh) "Refresh")))

  (let ((causal-lib-use-unicode t))
    (should (string-equal (causal-ediff-unicode-get :previous) "↑"))
    (should (string-equal (causal-ediff-unicode-get :next) "↓"))
    (should (string-equal (causal-ediff-unicode-get :scroll-to-right) "↤"))
    (should (string-equal (causal-ediff-unicode-get :scroll-to-left) "↦"))
    (should (string-equal (causal-ediff-unicode-get :refresh) "⟲"))))


(ert-deftest test-causal-ediff--display-filename ()
  (let* ((control "A: fred.foo")
         (filename "fred.foo")
         (slot "A")
         (extend nil)
         (fwidth 120)
         (result (causal-ediff--display-filename filename slot extend fwidth)))
    (should (string-equal control result)))

  (let* ((control "B: fred.foo")
         (filename "fred.foo")
         (slot "B")
         (extend nil)
         (fwidth 120)
         (result (causal-ediff--display-filename filename slot extend fwidth)))
    (should (string-equal control result)))

  (let* ((control "B: fred.foo                                              ")
         (filename "fred.foo")
         (slot "B")
         (extend t)
         (fwidth 120)
         (result (causal-ediff--display-filename filename slot extend fwidth)))
    (should (string-equal control result)))

  (let* ((control "B: fred.foo.~ABCDEF…~")
         (filename "fred.foo.~ABCDEF0123456~")
         (slot "B")
         (extend t)
         (fwidth 80)
         (result (causal-ediff--display-filename filename slot extend fwidth)))
    (should (string-equal control result)))

  (let* ((control
          "B: fred.foo.~ABCDEF0123456~                                                            ")
         (filename "fred.foo.~ABCDEF0123456~")
         (slot "B")
         (extend t)
         (fwidth 180)
         (result (causal-ediff--display-filename filename slot extend fwidth)))
    (should (string-equal control result)))

  (let* ((control
          "B: fred.foo.~ABCDEF0123456~")
         (filename "fred.foo.~ABCDEF0123456~")
         (slot "B")
         (extend nil)
         (fwidth 180)
         (result (causal-ediff--display-filename filename slot extend fwidth)))
    (should (string-equal control result))))

(provide 'test-causal-ediff-utils)
;;; test-causal-ediff-utils.el ends here
