;;; test-causal-csv.el --- Causal Make Tests -*- lexical-binding: t; -*-

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
(require 'causal-lib-test-utils)
(require 'causal-csv)

(ert-deftest test-causal-csv-tmenu ()
  (let ()
    (causalt-csv-setup)

    (cl-letf (
              (causalt-mock #'csv-backtab-command)
              (causalt-mock #'csv-tab-command)
              (causalt-mock #'previous-line)
              (causalt-mock #'next-line)
              (causalt-mock #'move-beginning-of-line)
              (causalt-mock #'move-end-of-line)
              (causalt-mock #'beginning-of-buffer)
              (causalt-mock #'end-of-buffer)
              (causalt-mock #'scroll-down-command)
              (causalt-mock #'scroll-up-command)
              (causalt-mock #'causal-csv-align-tmenu)
              (causalt-mock #'view-mode)
              (causalt-mock #'View-exit)
              (causalt-mock #'causal-lib-duplicate-file)
              (causalt-mock #'mark-sexp)
              (causalt-mock #'causal-editkit-copy-sexp)

              (causalt-mock #'occur)
              (causalt-mock #'causal-csv-kill-region-as-org-table)
              (causalt-mock #'causal-csv-settings-tmenu)

              (causalt-mock #'quit-window))

      (let ((test-vectors
             '(
               (:binding "S-TAB" :command csv-backtab-command)
               (:binding "TAB" :command csv-tab-command)
               (:binding "n" :command next-line)
               (:binding "p" :command previous-line)
               (:binding "C-e" :command move-end-of-line)
               (:binding "C-a" :command move-beginning-of-line)
               (:binding ">" :command end-of-buffer)
               (:binding "<" :command beginning-of-buffer)
               (:binding "C-v" :command scroll-up-command)
               (:binding "M-v" :command scroll-down-command)
               (:binding "a" :command causal-csv-align-tmenu)
               ;; (:binding "v" :command view-mode)
               ;; (:binding "e" :command View-exit)
               ;; (:binding "s" :command csv-sort-fields)

               (:binding "m" :command mark-sexp)
               (:binding "c" :command causal-editkit-copy-sexp)

               (:binding "o" :command occur)
               (:binding "," :command causal-csv-settings-tmenu)
               (:binding "q" :command quit-window)
               )))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-csv-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-csv-breakdown)))

(provide 'test-causal-csv)
;;; test-causal-csv.el ends here
