;;; test-causal-calc-utils.el --- Causal Calc Utils Tests  -*- lexical-binding: t; -*-

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
(require 'causal-calc)

(ert-deftest test-causal-calc-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal (causal-calc-unicode-get :inv) "1/x"))
    (should (string-equal (causal-calc-unicode-get :sqrt) "sqrt"))
    (should (string-equal (causal-calc-unicode-get :change-sign) "+/-"))
    (should (string-equal (causal-calc-unicode-get :power) "y^x"))
    (should (string-equal (causal-calc-unicode-get :abs) "|x|"))
    (should (string-equal (causal-calc-unicode-get :factorial) "!"))
    (should (string-equal (causal-calc-unicode-get :percent) "%"))
    (should (string-equal (causal-calc-unicode-get :percent-change) "% change"))
    (should (string-equal (causal-calc-unicode-get :pi) "pi"))
    (should (string-equal (causal-calc-unicode-get :e) "e"))
    (should (string-equal (causal-calc-unicode-get :ln) "ln"))
    (should (string-equal (causal-calc-unicode-get :lnp1) "ln(x+1)"))
    (should (string-equal (causal-calc-unicode-get :log10) "log10"))
    (should (string-equal (causal-calc-unicode-get :log) "log"))
    (should (string-equal (causal-calc-unicode-get :exp) "e^x"))
    (should (string-equal (causal-calc-unicode-get :expm1) "e^x - 1"))
    (should (string-equal (causal-calc-unicode-get :sin) "sin"))
    (should (string-equal (causal-calc-unicode-get :cos) "cos"))
    (should (string-equal (causal-calc-unicode-get :tan) "tan"))
    (should (string-equal (causal-calc-unicode-get :stack) "Stack"))
    (should (string-equal (causal-calc-unicode-get :arcsin) "arcsin"))
    (should (string-equal (causal-calc-unicode-get :arccos) "arccos"))
    (should (string-equal (causal-calc-unicode-get :arctan) "arctan"))
    (should (string-equal (causal-calc-unicode-get :degrees) "degrees"))
    (should (string-equal (causal-calc-unicode-get :radians) "radians"))
    (should (string-equal (causal-calc-unicode-get :hms) "HMS"))
    (should (string-equal (causal-calc-unicode-get :float) "float"))
    (should (string-equal (causal-calc-unicode-get :fraction) "fraction"))
    (should (string-equal (causal-calc-unicode-get :to) "to")))

  (let ((causal-lib-use-unicode t))
    (should (string-equal (causal-calc-unicode-get :inv) "1/𝑥"))
    (should (string-equal (causal-calc-unicode-get :sqrt) "√"))
    (should (string-equal (causal-calc-unicode-get :change-sign) "∓"))
    (should (string-equal (causal-calc-unicode-get :power) "𝑦ˣ"))
    (should (string-equal (causal-calc-unicode-get :abs) "|𝑥|"))
    (should (string-equal (causal-calc-unicode-get :factorial) " !"))
    (should (string-equal (causal-calc-unicode-get :percent) "%"))
    (should (string-equal (causal-calc-unicode-get :percent-change) " Δ%"))
    (should (string-equal (causal-calc-unicode-get :pi) "𝜋"))
    (should (string-equal (causal-calc-unicode-get :e) "𝑒"))
    (should (string-equal (causal-calc-unicode-get :ln) "𝑙𝑛"))
    (should (string-equal (causal-calc-unicode-get :lnp1) "𝑙𝑛(𝑥+𝟣)"))
    (should (string-equal (causal-calc-unicode-get :log10) "𝑙𝑜𝑔₁₀"))
    (should (string-equal (causal-calc-unicode-get :log) "𝑙𝑜𝑔ₐ(𝑥)"))
    (should (string-equal (causal-calc-unicode-get :exp) "𝑒ˣ"))
    (should (string-equal (causal-calc-unicode-get :expm1) "𝑒ˣ-𝟣"))
    (should (string-equal (causal-calc-unicode-get :sin) "𝑠𝑖𝑛"))
    (should (string-equal (causal-calc-unicode-get :cos) "𝑐𝑜𝑠"))
    (should (string-equal (causal-calc-unicode-get :tan) "𝑡𝑎𝑛"))
    (should (string-equal (causal-calc-unicode-get :stack) "≣"))
    (should (string-equal (causal-calc-unicode-get :arcsin) "𝑎𝑟𝑐𝑠𝑖𝑛"))
    (should (string-equal (causal-calc-unicode-get :arccos) "𝑎𝑟𝑐𝑐𝑜𝑠"))
    (should (string-equal (causal-calc-unicode-get :arctan) "𝑎𝑟𝑐𝑡𝑎𝑛"))
    (should (string-equal (causal-calc-unicode-get :degrees) "°"))
    (should (string-equal (causal-calc-unicode-get :radians) "𝑟𝑎𝑑"))
    (should (string-equal (causal-calc-unicode-get :hms) "ℎ𝑚𝑠"))
    (should (string-equal (causal-calc-unicode-get :float) "𝑓𝑙𝑜𝑎𝑡"))
    (should (string-equal (causal-calc-unicode-get :fraction) "𝑓𝑟𝑎𝑐𝑡𝑖𝑜𝑛"))
    (should (string-equal (causal-calc-unicode-get :to) "→"))))

(provide 'test-causal-calc-utils)
;;; test-causal-calc-utils.el ends here
