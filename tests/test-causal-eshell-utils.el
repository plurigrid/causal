;;; test-causal-eshell-utils.el --- Causal Make Utils Tests  -*- lexical-binding: t; -*-

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
(require 'causal-eshell-test-utils)
(require 'causal-eshell-utils)

(ert-deftest test-causal-eshell-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal (causal-eshell-unicode-get :previous) "Previous"))
    (should (string-equal (causal-eshell-unicode-get :next) "Next"))
    (should (string-equal (causal-eshell-unicode-get :forward) "Forward"))
    (should (string-equal (causal-eshell-unicode-get :backward) "Backward"))
    (should (string-equal (causal-eshell-unicode-get :repeat) "Repeat"))
    (should (string-equal (causal-eshell-unicode-get :clear) "Clear"))
    )

  (let ((causal-lib-use-unicode t))
    (should (string-equal (causal-eshell-unicode-get :previous) "↑"))
    (should (string-equal (causal-eshell-unicode-get :next) "↓"))
    (should (string-equal (causal-eshell-unicode-get :forward) "→"))
    (should (string-equal (causal-eshell-unicode-get :backward) "←"))
    (should (string-equal (causal-eshell-unicode-get :repeat) "⥅"))
    (should (string-equal (causal-eshell-unicode-get :clear) "⌫"))
    ))

(provide 'test-causal-eshell-utils)
;;; test-causal-eshell-utils.el ends here
