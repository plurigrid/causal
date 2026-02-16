;;; test-causal-csv-utils.el --- Causal Make Utils Tests  -*- lexical-binding: t; -*-

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
(require 'causal-csv-utils)

(ert-deftest test-causal-csv-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal (causal-csv-unicode-get :up) "Up"))
    (should (string-equal (causal-csv-unicode-get :down) "Down"))
    (should (string-equal (causal-csv-unicode-get :right) "Right"))
    (should (string-equal (causal-csv-unicode-get :left) "Left"))
    (should (string-equal (causal-csv-unicode-get :bol) "Begin"))
    (should (string-equal (causal-csv-unicode-get :eol) "End"))
    (should (string-equal (causal-csv-unicode-get :beginning-of-buffer) "Begin"))
    (should (string-equal (causal-csv-unicode-get :end-of-buffer) "End")))

  (let ((causal-lib-use-unicode t))
    (should (string-equal (causal-csv-unicode-get :up) "↑"))
    (should (string-equal (causal-csv-unicode-get :down) "↓"))
    (should (string-equal (causal-csv-unicode-get :right) "→"))
    (should (string-equal (causal-csv-unicode-get :left) "←"))
    (should (string-equal (causal-csv-unicode-get :bol) "⇤"))
    (should (string-equal (causal-csv-unicode-get :eol) "⇥"))
    (should (string-equal (causal-csv-unicode-get :beginning-of-buffer) "⇱"))
    (should (string-equal (causal-csv-unicode-get :end-of-buffer) "⇲"))))


(ert-deftest test-causal-csv-align-tmenu ()
  (let ((tmpfile "causal-csv-align-tmenu.txt"))
    (causalt-csv-setup)
    (cl-letf ((causalt-mock #'csv-align-mode)
              (causalt-mock #'causal-csv-align-auto)
              (causalt-mock #'causal-csv-align-left)
              (causalt-mock #'causal-csv-align-right)
              (causalt-mock #'causal-csv-align-centre))

      (let ((test-vectors
             '((:binding "t" :command csv-align-mode)
               (:binding "a" :command causal-csv-align-auto)
               (:binding "l" :command causal-csv-align-left)
               (:binding "r" :command causal-csv-align-right)
               (:binding "c" :command causal-csv-align-centre))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-csv-align-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-csv-breakdown)))

(provide 'test-causal-csv-utils)
;;; test-causal-csv-utils.el ends here
