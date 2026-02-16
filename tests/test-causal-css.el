;;; test-causal-css.el --- Causal Make Tests -*- lexical-binding: t; -*-

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
(require 'causal-lib-test-utils)
(require 'causal-css)

(ert-deftest test-causal-css-tmenu ()
  (let ((temp-file-name "causal-css-test.css"))
    (causalt-css-setup)

    (cl-letf ((causalt-mock #'css-lookup-symbol)
              (causalt-mock #'css-cycle-color-format)
              (causalt-mock #'fill-paragraph)
              (causalt-mock #'hl-line-mode)
              (causalt-mock #'causal-css-settings-tmenu)
              )



      (let ((test-vectors
             '((:binding "l" :command css-lookup-symbol)
               (:binding "c" :command css-cycle-color-format)
               (:binding "f" :command fill-paragraph)
               (:binding "h" :command hl-line-mode)
               (:binding "," :command causal-css-settings-tmenu)

               )))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-css-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-css-breakdown)))

(provide 'test-causal-css)
;;; test-causal-css.el ends here
