;;; test-causal-css-settings.el --- Causal Make Settings Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charles Y. Choi

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
(require 'causal-css-test-utils)
(require 'causal-css-settings)

(ert-deftest test-causal-css-settings-tmenu ()
  (let ()
    (cl-letf ((causalt-mock #'causal-css--customize-indent-offset)
              (causalt-mock #'causal-css--customize-group)
              (causalt-mock #'causal-css-about))

      (let ((test-vectors
             '((:binding "o" :command causal-css--customize-indent-offset)
               (:binding "G" :command causal-css--customize-group)
               (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
               (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
               (:binding "a" :command causal-css-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-css-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-causal-css-about ()
  (should (stringp (causal-css-about))))

(provide 'test-causal-css-settings)
;;; test-causal-css-setttings.el ends here
