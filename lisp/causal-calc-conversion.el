;;; causal-calc-conversion.el --- Causal Conversion Menu  -*- lexical-binding: t; -*-

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

(transient-define-prefix causal-calc-conversions-tmenu ()
  "Causal conversion functions menu."
  ["Conversions"

   ["Angle"
    ("d" "To Degrees" calc-to-degrees
     :description (lambda ()
                    (format "%s %s %s"
                            (causal-calc-unicode-get :radians)
                            (causal-calc-unicode-get :to)
                            (causal-calc-unicode-get :degrees)))
     :transient t)
    ("r" "To Radians" calc-to-radians
     :description (lambda ()
                    (format "%s %s %s"
                            (causal-calc-unicode-get :degrees)
                            (causal-calc-unicode-get :to)
                            (causal-calc-unicode-get :radians)))
     :transient t)]

    ["HMS"
     ("h" "To ℎ𝑚𝑠" calc-to-hms
      :description (lambda ()
                    (format "real %s %s"
                            (causal-calc-unicode-get :to)
                            (causal-calc-unicode-get :hms)))
      :transient t)
     ("H" "From ℎ𝑚𝑠" calc-from-hms
      :description (lambda ()
                     (format "%s %s real"
                             (causal-calc-unicode-get :hms)
                             (causal-calc-unicode-get :to)))
      :transient t)]

    ["Numeric"
     ("f" "To Float" calc-float
      :description (lambda ()
                     (format "%s %s"
                             (causal-calc-unicode-get :to)
                             (causal-calc-unicode-get :float)))
      :transient t)
     ("F" "To Fraction" calc-fraction
      :description (lambda ()
                     (format "%s %s"
                             (causal-calc-unicode-get :to)
                             (causal-calc-unicode-get :fraction)))
      :transient t)]]

  causal-calc-operators-group-row

  causal-calc-navigation-group)

(provide 'causal-calc-conversion)
;;; causal-calc-conversion.el ends here
