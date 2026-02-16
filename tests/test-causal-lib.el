;;; test-causal-lib.el --- Causal Avy Tests          -*- lexical-binding: t; -*-

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
(require 'causal-lib-test-utils)
(require 'causal-lib)

;; Test Unicode DB
(defconst causal-lib-unicode-db
  '((:scope . '("⬍" "#")))
  "Unicode symbol DB to use for Causal Transient menus.")

(ert-deftest test-causal-lib-use-unicode ()
  (should (symbolp 'causal-lib-use-unicode)))

(ert-deftest test-causal-lib-unicode-db ()
  (let* ((item (eval (alist-get :scope causal-lib-unicode-db))))
    (should (string-equal "#" (nth 1 item)))
    (should (string-equal "⬍" (nth 0 item)))))

(ert-deftest test-causal-lib-unicode-get ()
  (let ((causal-lib-use-unicode t))
    (should (string-equal "⬍" (causal-lib-unicode-db-get :scope causal-lib-unicode-db))))

  (let ((causal-lib-use-unicode nil))
    (should (string-equal "#" (causal-lib-unicode-db-get :scope causal-lib-unicode-db)))))

(defun causal-foo-unicode-get (key)
  "Example porcelain getter for KEY."
  (causal-lib-unicode-db-get key causal-lib-unicode-db))

(ert-deftest test-causal-foo-unicode-get ()
  (let ((causal-lib-use-unicode t))
    (should (string-equal "⬍" (causal-foo-unicode-get :scope))))

  (let ((causal-lib-use-unicode nil))
    (should (string-equal "#" (causal-foo-unicode-get :scope)))))

;; Test Labels
(ert-deftest test-causal-lib--variable-to-checkbox ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal "[x]" (causal-lib--variable-to-checkbox t)))
    (should (string-equal "[ ]" (causal-lib--variable-to-checkbox nil))))

  (let ((causal-lib-use-unicode t))
    (should (string-equal "☑︎" (causal-lib--variable-to-checkbox t)))
    (should (string-equal "◻︎" (causal-lib--variable-to-checkbox nil)))))

(ert-deftest test-causal-lib--prefix-label ()
  (let ((label "foo")
        (prefix "bar"))
    (should (string-equal "bar foo" (causal-lib--prefix-label label prefix)))))

(ert-deftest test-causal-lib--suffix-label ()
  (let ((label "foo")
        (suffix "bar"))
    (should (string-equal "foo bar" (causal-lib--suffix-label label suffix)))))

(ert-deftest test-causal-lib-checkbox-label ()
  (let ((causal-lib-use-unicode nil)
        (label "bingo"))
    (should (string-equal "[x] bingo" (causal-lib-checkbox-label t label)))
    (should (string-equal "[ ] bingo" (causal-lib-checkbox-label nil label))))

  (let ((causal-lib-use-unicode t)
        (label "bingo"))
    (should (string-equal "☑︎ bingo" (causal-lib-checkbox-label t label)))
    (should (string-equal "◻︎ bingo" (causal-lib-checkbox-label nil label)))))

;; Test Predicates
(ert-deftest test-causal-lib-display-line-numbers-mode-p ()
  (let ((display-line-numbers nil))
    (should (not (causal-lib-display-line-numbers-mode-p))))

  (let ((display-line-numbers 'relative))
    (should (causal-lib-display-line-numbers-mode-p))))

(ert-deftest test-causal-lib-buffer-writeable-p ()
  (with-temp-buffer
    (should (causal-lib-buffer-writeable-p)))

  (with-temp-buffer
    (read-only-mode)
    (should (not (causal-lib-buffer-writeable-p)))))

;; Dunno why this works interactively but not in batch
;; (ert-deftest test-causal-lib-buffer-writeable-and-region-active-p ()
;;   (with-temp-buffer
;;     (set-mark 0)
;;     (insert "hey there")
;;     (end-of-buffer)
;;     (should (causal-lib-buffer-writeable-and-region-active-p))))

(ert-deftest test-causal-lib-hide-navigation ()
  (should (symbolp 'causal-lib-hide-navigation)))

(ert-deftest test-causal-lib-hide-navigation-p ()
  (let ((causal-lib-hide-navigation t))
    (should (equal t (causal-lib-hide-navigation-p))))

  (let ((causal-lib-hide-navigation nil))
    (should (equal nil (causal-lib-hide-navigation-p)))))


(ert-deftest test-causal-lib-quit-all-hide-navigation-p ()
  (let ((causal-lib-hide-navigation nil)
        (transient--stack nil))
    (should (equal (causal-lib-quit-all-hide-navigation-p) t)))

  (let ((causal-lib-hide-navigation nil)
        (transient--stack t))
    (should (equal (causal-lib-quit-all-hide-navigation-p) nil)))

  (let ((causal-lib-hide-navigation t)
        (transient--stack nil))
    (should (equal (causal-lib-quit-all-hide-navigation-p) t)))

  (let ((causal-lib-hide-navigation t)
        (transient--stack t))
    (should (equal (causal-lib-quit-all-hide-navigation-p) t))))

(provide 'test-causal-lib)
;;; test-causal-lib.el ends here
