;;; test-causal-ediff-settings.el --- Causal Elisp Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-ediff-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-ediff-settings)

(ert-deftest test-causal-ediff-settings-tmenu ()
  (let ()
    (cl-letf ((causalt-mock #'causal-ediff-customize-group)
              (causalt-mock #'causal-ediff-customize-ediff-keep-variants)
              (causalt-mock #'causal-ediff-customize-ediff-split-window-function)
              (causalt-mock #'causal-ediff-customize-ediff-window-setup-function)
              (causalt-mock #'causal-lib-customize-causal-lib-hide-navigation)
              (causalt-mock #'causal-lib-customize-causal-lib-use-unicode)
              (causalt-mock #'causal-ediff-about))

      (let ((test-vectors
             '(
               (:binding "k" :command causal-ediff-customize-ediff-keep-variants)
               (:binding "w" :command causal-ediff-customize-ediff-window-setup-function)
               (:binding "s" :command causal-ediff-customize-ediff-split-window-function)
               (:binding "G" :command causal-ediff-customize-group)
               (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
               (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
               (:binding "a" :command causal-ediff-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-ediff-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-causal-ediff-about ()
  (should (stringp (causal-ediff-about))))

(provide 'test-causal-ediff-settings)
;;; test-causal-ediff-setttings.el ends here
