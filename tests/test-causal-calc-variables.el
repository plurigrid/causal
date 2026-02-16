;;; test-causal-calc-variables.el --- Test Causal Variables  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Charles Y. Choi

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
(require 'causal-calc-variables)


(ert-deftest test-causal-calc-variable-crud-tmenu ()
  (causalt-calc-setup)
  (let ((test-vectors '(("s" . causal-calc--store)
                        ("r" . causal-calc--recall)
                        ("c" . causal-calc--unstore)
                        ("e" . causal-calc--edit-variable)
                        ("o" . causal-calc--copy-variable)
                        ("x" . causal-calc--store-exchange)
                        ("p" . causal-calc--permanent-variable)
                        ("O" . causal-calc-open-settings-file)
                        ("i" . causal-calc--insert-variables))))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-variable-crud-tmenu
                                     '(lambda () (random 5000))))
  )



(provide 'test-causal-calc-variables)
;;; test-causal-calc-variables.el ends here
