;;; test-causal-eshell-settings.el --- Causal Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-eshell-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-eshell-settings)

(ert-deftest test-causal-eshell-settings-tmenu ()
  (let ()
    (cl-letf ((causalt-mock #'causal-eshell--customize-group)
              (causalt-mock #'causal-eshell-about))

      (let ((test-vectors
             '((:binding "G" :command causal-eshell--customize-group)
               (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
               (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
               (:binding "a" :command causal-eshell-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-eshell-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-causal-eshell-about ()
  (should (stringp (causal-eshell-about))))

(provide 'test-causal-eshell-settings)
;;; test-causal-eshell-setttings.el ends here
