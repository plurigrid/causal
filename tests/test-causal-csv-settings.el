;;; test-causal-csv-settings.el --- Causal Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-csv-test-utils)
(require 'causal-csv-settings)

(ert-deftest test-causal-csv-settings-tmenu ()
  (let ()
    (cl-letf ((causalt-mock #'causal-csv--customize-align-style)
              (causalt-mock #'causal-csv--customize-separators)
              (causalt-mock #'causal-csv--customize-invisibility-default)
              (causalt-mock #'causal-csv--customize-group)
              (causalt-mock #'causal-csv--customize-header-lines)
              (causalt-mock #'causal-csv--customize-comment-start-default)
              (causalt-mock #'causal-csv--customize-field-quotes)
              (causalt-mock #'causal-csv--customize-align-min-width)
              (causalt-mock #'causal-csv--customize-align-max-width)
              (causalt-mock #'causal-csv-about))

      (let ((test-vectors
             '(
               (:binding "A" :command causal-csv--customize-align-style)
               (:binding "s" :command causal-csv--customize-separators)
               (:binding "i" :command causal-csv--customize-invisibility-default)
               (:binding "G" :command causal-csv--customize-group)
               (:binding "h" :command causal-csv--customize-header-lines)
               (:binding "c" :command causal-csv--customize-comment-start-default)
               (:binding "f" :command causal-csv--customize-field-quotes)
               (:binding "w" :command causal-csv--customize-align-min-width)
               (:binding "W" :command causal-csv--customize-align-max-width)

               (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
               (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
               (:binding "a" :command causal-csv-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-csv-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-causal-csv-about ()
  (should (stringp (causal-csv-about))))

(provide 'test-causal-csv-settings)
;;; test-causal-csv-setttings.el ends here
