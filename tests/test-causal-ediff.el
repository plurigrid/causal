;;; test-causal-ediff.el --- Causal Make Tests -*- lexical-binding: t; -*-

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
(require 'causal-ediff-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-ediff)

;; Gah, testing causal-ediff-tmenu might be too hard.
;; Resorting to manual testing.

;; (ert-deftest test-causal-ediff-tmenu ()
;;   (let ((tmpfile "causal-ediff-tmenu.txt"))
;;     (causalt-ediff-setup)
;;     (cl-letf (;;((symbol-function #') (lambda () t))
;;               (causalt-mock #'ediff-previous-difference)
;;               (causalt-mock #'ediff-next-difference)
;;               ;;(causalt-mock #')
;;               )

;;       (let ((test-vectors
;;              '((:binding "p" :command ediff-previous-difference)
;;                (:binding "n" :command ediff-next-difference)
;;                ;;(:binding "" :command ediff-main-)
;;                )))

;;         (causalt-suffix-testcase-runner test-vectors
;;                                         #'causal-ediff-tmenu
;;                                         '(lambda () (random 5000)))))
;;     (causalt-ediff-breakdown)))

(provide 'test-causal-ediff)
;;; test-causal-ediff.el ends here
