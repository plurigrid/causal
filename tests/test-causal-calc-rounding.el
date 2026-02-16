;;; test-causal-calc-rounding.el --- Causal Rounding Menu Tests  -*- lexical-binding: t; -*-

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
(require 'causal-calc-rounding)

(ert-deftest test-causal-calc-rounding-tmenu ()
  (causalt-calc-setup)

  (let* ((test-vectors
          '(("r" . causal-calc--round)
            ("f" . causal-calc--floor)
            ("c" . causal-calc--ceiling)
            ("t" . causal-calc--trunc)))
         (test-vectors (append test-vectors causalt-test-operators-group)))

    ;;(pp test-vectors)
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-rounding-tmenu
                                     '(lambda () (random 5000))))
  (causalt-calc-breakdown t t))

(provide 'test-causal-calc-rounding)
;;; test-causal-calc-rounding.el ends here
