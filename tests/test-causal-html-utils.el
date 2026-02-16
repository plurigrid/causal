;;; test-causal-html-utils.el --- Causal Make Utils Tests  -*- lexical-binding: t; -*-

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
(require 'causal-html-test-utils)
(require 'causal-html-utils)

(ert-deftest test-causal-html-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal (causal-html-unicode-get :previous) "Previous"))
    (should (string-equal (causal-html-unicode-get :next) "Next")))

  (let ((causal-lib-use-unicode t))
    (should (string-equal (causal-html-unicode-get :previous) "↑"))
    (should (string-equal (causal-html-unicode-get :next) "↓"))))

(provide 'test-causal-html-utils)
;;; test-causal-html-utils.el ends here
