;;; test-causal-ibuffer-utils.el --- Causal IBuffer Utils Tests  -*- lexical-binding: t; -*-

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
(require 'causal-ibuffer-test-utils)
(require 'causal-ibuffer-utils)

(ert-deftest test-causal-ibuffer-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal (causal-ibuffer-unicode-get :previous) "Previous"))
    (should (string-equal (causal-ibuffer-unicode-get :next) "Next"))
    (should (string-equal (causal-ibuffer-unicode-get :jump) "Jump"))
    (should (string-equal (causal-ibuffer-unicode-get :marked) "Marked"))
    (should (string-equal (causal-ibuffer-unicode-get :group) "Group"))
    (should (string-equal (causal-ibuffer-unicode-get :jumpgroup) "Jump to Group")))

  (let ((causal-lib-use-unicode t))
    (should (string-equal (causal-ibuffer-unicode-get :previous) "↑"))
    (should (string-equal (causal-ibuffer-unicode-get :next) "↓"))
    (should (string-equal (causal-ibuffer-unicode-get :jump) "🚀"))
    (should (string-equal (causal-ibuffer-unicode-get :marked) "❯"))
    (should (string-equal (causal-ibuffer-unicode-get :group) "[]"))
    (should (string-equal (causal-ibuffer-unicode-get :jumpgroup) "🚀[]"))))

(provide 'test-causal-ibuffer-utils)
;;; test-causal-ibuffer-utils.el ends here
