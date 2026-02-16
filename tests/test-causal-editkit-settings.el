;;; test-causal-editkit-settings.el --- Causal IBuffer Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-editkit-test-utils)
(require 'causal-editkit-settings)

(ert-deftest test-causal-editkit-settings-tmenu ()
  (causalt-editkit-setup)
  (cl-letf ((causalt-mock #'auto-fill-mode)
            (causalt-mock #'indent-tabs-mode)
            (causalt-mock #'set-fill-column))
    (let ((test-vectors
           '((:binding "f" :command auto-fill-mode)
             (:binding "t" :command indent-tabs-mode)
             (:binding "C" :command set-fill-column)
             (:binding "." :command causal-editkit--customize-sentence-end-double-space)
             (:binding "p" :command causal-editkit--customize-save-place-mode)
             (:binding "h" :command causal-editkit--customize-savehist-mode)
             (:binding "S" :command causal-editkit--customize-delete-selection-mode)
             (:binding "N" :command causal-editkit--customize-require-final-newline)
             (:binding "v" :command causal-editkit--customize-view-read-only)
             (:binding "F" :command causal-editkit--customize-global-auto-revert-mode)
             (:binding "B" :command causal-editkit--customize-global-auto-revert-non-file-buffers)
             (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
             (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
             (:binding "a" :command causal-editkit-about))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-editkit-settings-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-editkit-breakdown))

(ert-deftest test-causal-editkit-about ()
  (should (stringp (causal-editkit-about))))

(provide 'test-causal-editkit-settings)
;;; test-causal-editkit-setttings.el ends here
