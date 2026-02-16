;;; test-causal-timezone.el --- Causal Make Tests -*- lexical-binding: t; -*-

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
(require 'causal-lib-test-utils)
(require 'causal-timezone)

(ert-deftest test-causal-timezone-tmenu ()
  (let ()
    (causalt-timezone-setup)
    (cl-letf ((causalt-mock #'causal-timezone-local-time-to-remote)
              (causalt-mock #'causal-timezone-remote-time-to-local)
              (causalt-mock #'causal-timezone-planner))

      (let ((test-vectors
             '((:binding "l" :command causal-timezone-local-time-to-remote)
               (:binding "r" :command causal-timezone-remote-time-to-local)
               (:binding "z" :command causal-timezone-planner))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-timezone-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-timezone-breakdown)))

(provide 'test-causal-timezone)
;;; test-causal-timezone.el ends here
