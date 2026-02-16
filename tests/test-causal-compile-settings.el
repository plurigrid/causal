;;; test-causal-compile-settings.el --- Causal Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-compile-test-utils)
(require 'causal-compile-settings)

(ert-deftest test-causal-compile-settings-tmenu ()
  (let ()
    (cl-letf ((causalt-mock #'causal-compile--customize-group)
              (causalt-mock #'causal-compile--customize-compilation-scroll-output)
              (causalt-mock #'causal-compile--customize-compilation-auto-jump-to-first-error)
              (causalt-mock #'causal-compile--customize-compilation-max-output-line-length)
              (causalt-mock #'causal-compile-about))

      (let ((test-vectors
             '((:binding "s" :command causal-compile--customize-compilation-scroll-output)
               (:binding "e" :command causal-compile--customize-compilation-auto-jump-to-first-error)
               (:binding "m" :command causal-compile--customize-compilation-max-output-line-length)
               (:binding "G" :command causal-compile--customize-group)
               (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
               (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
               (:binding "a" :command causal-compile-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-compile-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-causal-compile-about ()
  (should (stringp (causal-compile-about))))

(provide 'test-causal-compile-settings)
;;; test-causal-compile-setttings.el ends here
