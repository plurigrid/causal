;;; causal-calc-logarithmic.el --- Causal Logarithmic Menu  -*- lexical-binding: t; -*-

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
(require 'calc)
(require 'transient)
(require 'causal-lib)
(require 'causal-calc-utils)

(transient-define-prefix causal-calc-logarithmic-tmenu ()
  "Causal logarithmic functions."
  ["Logarithmic Functions"
   ["Logarithm"
    ("l" "𝑙𝑛" calc-ln
     :description (lambda () (causal-calc-unicode-get :ln))
     :transient t)
    ("p" "𝑙𝑛(𝑥+𝟣)" calc-lnp1
     :description (lambda () (causal-calc-unicode-get :lnp1))
     :transient t)
    ("1" "𝑙𝑜𝑔₁₀" calc-log10
     :description (lambda () (causal-calc-unicode-get :log10))
     :transient t)
    ("L" "𝑙𝑜𝑔ₐ(𝑥)" calc-log
     :description (lambda () (causal-calc-unicode-get :log))
     :transient t)]

   ["Exponential"
    ("^" "𝑒ˣ" calc-exp
     :description (lambda () (causal-calc-unicode-get :exp))
     :transient t)
    ("m" "𝑒ˣ-𝟣" calc-expm1
     :description (lambda () (causal-calc-unicode-get :expm1))
     :transient t)]

   ["Constant"
    ("e" "𝑒" causal-calc--e-constant
     :description (lambda () (causal-calc-unicode-get :e))
     :transient t)]]

  causal-calc-operators-group-row
  causal-calc-navigation-group)

(provide 'causal-calc-logarithmic)
;;; causal-calc-logarithmic.el ends here
