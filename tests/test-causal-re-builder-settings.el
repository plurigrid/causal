;;; test-causal-re-builder-settings.el --- Causal Re-Builder Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-re-builder-test-utils)
(require 'causal-re-builder-settings)

(ert-deftest test-causal-re-builder-settings-tmenu-bindings ()
  (causalt-setup)
  (let ((test-vectors (list)))
    (push (causalt-suffix-test-vector "G" #'causal-re-builder--customize-group) test-vectors)

    (push (causalt-suffix-test-vector "u" #'causal-lib-customize-causal-lib-use-unicode) test-vectors)
    (push (causalt-suffix-test-vector "n" #'causal-lib-customize-causal-lib-hide-navigation) test-vectors)
    (push (causalt-suffix-test-vector "a" #'causal-re-builder-about) test-vectors)


    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-re-builder-settings-tmenu
                                     '(lambda () (random 5000))))
  (causalt-breakdown t))

(ert-deftest test-causal-re-builder-about ()
  (should (stringp (causal-re-builder-about))))

(provide 'test-causal-re-builder-settings)
;;; test-causal-re-builder-setttings.el ends here
