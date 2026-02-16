;;; test-causal-man-settings.el --- Causal Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-man-test-utils)
(require 'causal-man-settings)

(ert-deftest test-causal-man-settings-tmenu ()
  (let ()
    (cl-letf ((causalt-mock #'causal-man--customize-group)
              (causalt-mock #'causal-man-about)
              (causalt-mock #'causal-man--customize-man-switches)
              (causalt-mock #'causal-man--customize-man-prefer-synchronous-call)
              (causalt-mock #'causal-man--customize-man-support-remote-systems))

      (let ((test-vectors
             '((:binding "s" :command causal-man--customize-man-switches)
               (:binding "S" :command causal-man--customize-man-prefer-synchronous-call)
               (:binding "r" :command causal-man--customize-man-support-remote-systems)
               (:binding "G" :command causal-man--customize-group)
               (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
               (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
               (:binding "a" :command causal-man-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-man-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-causal-man-about ()
  (should (stringp (causal-man-about))))

(provide 'test-causal-man-settings)
;;; test-causal-man-setttings.el ends here
