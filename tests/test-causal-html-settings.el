;;; test-causal-html-settings.el --- Causal Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-html-test-utils)
(require 'causal-html-settings)

(ert-deftest test-causal-html-settings-tmenu ()
  (let ()
    (cl-letf (
              (causalt-mock #'causal-html--customize-sgml-basic-offset)
              (causalt-mock #'causal-html--customize-sgml-attribute-offset)
              (causalt-mock #'causal-html--customize-html-ts-mode-indent-offset)
              (causalt-mock #'causal-html--customize-group-sgml)
              (causalt-mock #'causal-html-about))

      (let ((test-vectors
             '((:binding "b" :command causal-html--customize-sgml-basic-offset)
               (:binding "A" :command causal-html--customize-sgml-attribute-offset)
               (:binding "G" :command causal-html--customize-group-sgml)
               (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
               (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
               (:binding "a" :command causal-html-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-html-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-causal-html-about ()
  (should (stringp (causal-html-about))))

(provide 'test-causal-html-settings)
;;; test-causal-html-setttings.el ends here
