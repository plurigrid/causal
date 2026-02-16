;;; test-causal-timezone-settings.el --- Causal Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-timezone-test-utils)
(require 'causal-timezone-settings)

(ert-deftest test-causal-timezone-settings-tmenu ()
  (let ()
    (causalt-timezone-setup)
    (cl-letf ((causalt-mock #'causal-timezone-about)
              (causalt-mock #'causal-timezone--customize-working-hours-range)
              (causalt-mock #'causal-timezone--customize-working-hour-glyph)
              (causalt-mock #'causal-timezone--customize-planner-working-highlight)
              (causalt-mock #'causal-timezone--customize-convert-timestamp-format)
              (causalt-mock #'causal-timezone--customize-datestamp-format)
              (causalt-mock #'causal-timezone--customize-zone-info-database)
              (causalt-mock #'causal-timezone--describe-format-time-string))

      (let ((test-vectors
             '((:binding "r" :command causal-timezone--customize-working-hours-range)
               (:binding "g" :command causal-timezone--customize-working-hour-glyph)
               (:binding "F" :command causal-timezone--customize-planner-working-highlight)
               (:binding "c" :command causal-timezone--customize-convert-timestamp-format)
               (:binding "p" :command causal-timezone--customize-datestamp-format)
               (:binding "f" :command causal-timezone--describe-format-time-string)
               (:binding "D" :command causal-timezone--customize-zone-info-database)
               (:binding "a" :command causal-timezone-about)
               )))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-timezone-settings-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-timezone-breakdown)))

(ert-deftest test-causal-timezone-about ()
  (should (stringp (causal-timezone-about))))

(provide 'test-causal-timezone-settings)
;;; test-causal-timezone-setttings.el ends here
