;;; causal-calc-units.el --- Causal Units                 -*- lexical-binding: t; -*-

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

(transient-define-prefix causal-calc-units-tmenu ()
  "Causal unit conversions menu."
  [["Unit Conversions"
   ("c" "Convert" calc-convert-units :transient t)
   ("t" "Convert Temperature" calc-convert-temperature :transient t)
   ("b" "Convert to Base Unit" calc-base-units :transient t)
   ;; TODO: display current autorange state
   ;;("a" "Autorange" calc-autorange-units :transient t)
   ("r" "Remove Units" calc-remove-units :transient t)
   ("x" "Extract Units" calc-extract-units :transient t)
   ("v" "View Units" calc-view-units-table :transient t)]
   causal-calc-operators-group]

  causal-calc-navigation-group)

(provide 'causal-calc-units)
;;; causal-calc-units.el ends here
