;;; test-causal-elisp-settings.el --- Causal Elisp Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-elisp-test-utils)
(require 'causal-elisp-settings)

(ert-deftest test-causal-elisp-settings-tmenu ()
  (let ()
    (cl-letf ((causalt-mock #'causal-lib-customize-causal-lib-hide-navigation)
              (causalt-mock #'causal-lib-customize-causal-lib-use-unicode)
              (causalt-mock #'causal-elisp-about))

      (let ((test-vectors
             '((:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
               (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
               (:binding "a" :command causal-elisp-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-elisp-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-causal-elisp-about ()
  (should (stringp (causal-elisp-about))))

(provide 'test-causal-elisp-settings)
;;; test-causal-elisp-setttings.el ends here
