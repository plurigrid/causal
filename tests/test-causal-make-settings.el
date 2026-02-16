;;; test-causal-make-settings.el --- Causal Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-make-test-utils)
(require 'causal-make-settings)

(ert-deftest test-causal-make-settings-tmenu ()
  (let ((tmpfile "causal-make-settings-tmenu.txt"))
    (causalt-make-setup)
    (cl-letf (;;((symbol-function #') (lambda () t))
              (causalt-mock #'causal-make--customize-group))

      (let ((test-vectors
             '((:binding "G" :command causal-make--customize-group)
               (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
               (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
               (:binding "a" :command causal-make-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-make-settings-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-make-breakdown)))

(ert-deftest test-causal-make-about ()
  (should (stringp (causal-make-about))))

(provide 'test-causal-make-settings)
;;; test-causal-make-setttings.el ends here
