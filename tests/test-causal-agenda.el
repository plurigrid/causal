;;; test-causal-agenda.el --- Causal Agenda Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Y. Choi

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
(require 'causal-agenda-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-agenda)


(ert-deftest test-causal-agenda-tmenu ()
  (causalt-agenda-setup)
  (cl-letf (((symbol-function #'causal-agenda-headlinep) (lambda () t))
            ((symbol-function #'causal-agenda-type-datep) (lambda () t))
            ((symbol-function #'causal-agenda-type-agendap) (lambda () t))
            ((symbol-function #'org-clocking-p) (lambda () t))
            (causalt-mock #'org-agenda-previous-line)
            (causalt-mock #'org-agenda-next-line)
            (causalt-mock #'org-agenda-previous-item)
            (causalt-mock #'org-agenda-next-item)
            (causalt-mock #'org-agenda-previous-date-line)
            (causalt-mock #'org-agenda-next-date-line)
            (causalt-mock #'org-agenda-earlier)
            (causalt-mock #'org-agenda-later)
            (causalt-mock #'org-agenda-redo)
            (causalt-mock #'org-agenda-redo-all)
            (causalt-mock #'org-agenda-goto-date)
            (causalt-mock #'org-agenda-clock-goto)

            (causalt-mock #'org-agenda-filter)
            (causalt-mock #'org-agenda-filter-by-regexp)
            (causalt-mock #'org-agenda-filter-by-tag)
            (causalt-mock #'org-agenda-filter-by-top-headline)
            (causalt-mock #'org-agenda-filter-by-category)
            (causalt-mock #'org-agenda-filter-by-effort)
            (causalt-mock #'org-agenda-filter-remove-all)

            (causalt-mock #'org-agenda-day-view)
            (causalt-mock #'org-agenda-week-view)
            (causalt-mock #'org-agenda-month-view)
            (causalt-mock #'org-agenda-fortnight-view)
            (causalt-mock #'org-agenda-year-view)
            (causalt-mock #'org-save-all-org-buffers)
            (causalt-mock #'org-capture)
            (causalt-mock #'org-agenda)
            (causalt-mock #'bookmark-jump)

            (causalt-mock #'org-agenda-goto-calendar)
            (causalt-mock #'org-agenda-sunrise-sunset)
            (causalt-mock #'org-agenda-phases-of-moon)
            (causalt-mock #'org-agenda-holidays)
            (causalt-mock #'org-agenda-undo)
            (causalt-mock #'org-info-find-node)
            (causalt-mock #'org-timer-set-timer)
            (causalt-mock #'org-agenda-quit))

    (let ((test-vectors
           '((:binding "p" :command org-agenda-previous-line)
             (:binding "n" :command org-agenda-next-line)
             (:binding "P" :command org-agenda-previous-item)
             (:binding "N" :command org-agenda-next-item)
             (:binding "M-p" :command org-agenda-previous-date-line)
             (:binding "M-n" :command org-agenda-next-date-line)
             (:binding "b" :command org-agenda-earlier)
             (:binding "f" :command org-agenda-later)
             (:binding "r" :command org-agenda-redo)
             (:binding "g" :command org-agenda-redo-all)
             (:binding "j" :command org-agenda-goto-date)
             (:binding "M-j" :command org-agenda-clock-goto)

             (:binding "/" :command org-agenda-filter)
             (:binding "=" :command org-agenda-filter-by-regexp)
             (:binding "\\" :command org-agenda-filter-by-tag)
             (:binding "^" :command org-agenda-filter-by-top-headline)
             (:binding "<" :command org-agenda-filter-by-category)
             (:binding "_" :command org-agenda-filter-by-effort)
             (:binding "|" :command org-agenda-filter-remove-all)

             (:binding "d" :command org-agenda-day-view)
             (:binding "w" :command org-agenda-week-view)
             (:binding "m" :command org-agenda-month-view)
             (:binding "y" :command org-agenda-year-view)
             (:binding "o" :command causal-agenda-operations-tmenu)
             (:binding "M" :command causal-agenda-mark-tmenu)

             (:binding "s" :command org-save-all-org-buffers)
             (:binding "k" :command org-capture)
             (:binding "a" :command org-agenda)
             (:binding "J" :command bookmark-jump)

             (:binding ";" :command org-timer-set-timer)
             (:binding "c" :command org-agenda-goto-calendar)
             (:binding "l" :command causal-agenda-almanac-tmenu)

             ;;(:binding "" :command org-agenda-undo)
             (:binding "I" :command org-info-find-node)

             (:binding "," :command causal-agenda-settings-tmenu)
             (:binding "q" :command org-agenda-quit))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-agenda-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-agenda-breakdown t))

(ert-deftest test-causal-agenda-almanac-tmenu ()
  (causalt-agenda-setup)
  (cl-letf (((symbol-function #'causal-agenda-headlinep) (lambda () t))
            ((symbol-function #'causal-agenda-type-datep) (lambda () t))
            ((symbol-function #'causal-agenda-type-agendap) (lambda () t))
            ((symbol-function #'org-clocking-p) (lambda () t))
            (causalt-mock #'org-agenda-sunrise-sunset)
            (causalt-mock #'org-agenda-phases-of-moon)
            (causalt-mock #'org-agenda-holidays)

            (causalt-mock #'org-agenda-previous-line)
            (causalt-mock #'org-agenda-next-line)
            (causalt-mock #'org-agenda-previous-item)
            (causalt-mock #'org-agenda-next-item)
            (causalt-mock #'org-agenda-previous-date-line)
            (causalt-mock #'org-agenda-next-date-line)
            (causalt-mock #'org-agenda-earlier)
            (causalt-mock #'org-agenda-later)
            (causalt-mock #'org-agenda-redo)
            (causalt-mock #'org-agenda-redo-all)
            (causalt-mock #'org-agenda-goto-date)
            (causalt-mock #'org-agenda-clock-goto))

    (let ((test-vectors
           '((:binding "S" :command org-agenda-sunrise-sunset)
             (:binding "M" :command org-agenda-phases-of-moon)
             (:binding "H" :command org-agenda-holidays)

             (:binding "p" :command org-agenda-previous-line)
             (:binding "n" :command org-agenda-next-line)
             (:binding "P" :command org-agenda-previous-item)
             (:binding "N" :command org-agenda-next-item)
             (:binding "M-p" :command org-agenda-previous-date-line)
             (:binding "M-n" :command org-agenda-next-date-line)
             (:binding "b" :command org-agenda-earlier)
             (:binding "f" :command org-agenda-later)
             (:binding "r" :command org-agenda-redo)
             (:binding "g" :command org-agenda-redo-all)
             (:binding "j" :command org-agenda-goto-date)
             (:binding "M-j" :command org-agenda-clock-goto))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-agenda-almanac-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-agenda-breakdown t t))



(ert-deftest test-causal-agenda-operations-tmenu ()
  (causalt-agenda-setup)
  (cl-letf (((symbol-function #'causal-agenda-headlinep) (lambda () t))
            ((symbol-function #'causal-agenda-type-datep) (lambda () t))
            ((symbol-function #'causal-agenda-type-agendap) (lambda () t))
            ((symbol-function #'org-clocking-p) (lambda () t))
            (causalt-mock #'org-agenda-todo)
            (causalt-mock #'org-agenda-schedule)
            (causalt-mock #'org-agenda-deadline)
            (causalt-mock #'org-agenda-set-tags)
            (causalt-mock #'org-agenda-priority-up)
            (causalt-mock #'org-agenda-priority-down)
            (causalt-mock #'org-agenda-refile)
            (causalt-mock #'org-agenda-add-note)
            (causalt-mock #'org-agenda-archive-default-with-confirmation)

            (causalt-mock #'org-clock-cancel)
            (causalt-mock #'org-clock-modify-effort-estimate)
            (causalt-mock #'causal-agenda-clock-out)

            (causalt-mock #'org-agenda-previous-line)
            (causalt-mock #'org-agenda-next-line)
            (causalt-mock #'org-agenda-previous-item)
            (causalt-mock #'org-agenda-next-item)
            (causalt-mock #'org-agenda-previous-date-line)
            (causalt-mock #'org-agenda-next-date-line)
            (causalt-mock #'org-agenda-earlier)
            (causalt-mock #'org-agenda-later)
            (causalt-mock #'org-agenda-redo)
            (causalt-mock #'org-agenda-redo-all)
            (causalt-mock #'org-agenda-goto-date)
            (causalt-mock #'org-agenda-clock-goto)
            (causalt-mock #'org-agenda-set-property)
            (causalt-mock #'org-agenda-bulk-action))

    (let ((test-vectors
           '((:binding "t" :command org-agenda-todo)
             (:binding "B" :command org-agenda-bulk-action)
             (:binding "s" :command org-agenda-schedule)
             (:binding "d" :command org-agenda-deadline)
             (:binding ":" :command org-agenda-set-tags)
             (:binding "+" :command org-agenda-priority-up)
             (:binding "-" :command org-agenda-priority-down)
             (:binding "R" :command org-agenda-refile)
             (:binding "z" :command org-agenda-add-note)
             (:binding "S" :command org-agenda-set-property)
             (:binding "A" :command org-agenda-archive-default-with-confirmation)

             (:binding "I" :command causal-agenda-clock-in)

             ;; TODO: figure out how to test :inapt switches
             (:binding "O" :command causal-agenda-clock-out)
             (:binding "x" :command causal-agenda-clock-cancel)
             (:binding "m" :command org-clock-modify-effort-estimate)

             (:binding "p" :command org-agenda-previous-line)
             (:binding "n" :command org-agenda-next-line)
             (:binding "P" :command org-agenda-previous-item)
             (:binding "N" :command org-agenda-next-item)
             (:binding "M-p" :command org-agenda-previous-date-line)
             (:binding "M-n" :command org-agenda-next-date-line)
             (:binding "b" :command org-agenda-earlier)
             (:binding "f" :command org-agenda-later)
             (:binding "r" :command org-agenda-redo)
             (:binding "g" :command org-agenda-redo-all)
             (:binding "j" :command org-agenda-goto-date)
             (:binding "M-j" :command org-agenda-clock-goto))))

      (defun org-clocking-p ()
        t)

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-agenda-operations-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-agenda-breakdown t t))


(ert-deftest test-causal-agenda-mark-tmenu ()
  (causalt-agenda-setup)
  (cl-letf (((symbol-function #'causal-agenda-headlinep) (lambda () t))
            ((symbol-function #'causal-agenda-type-datep) (lambda () t))
            ((symbol-function #'causal-agenda-type-agendap) (lambda () t))
            ((symbol-function #'org-clocking-p) (lambda () t))
            (causalt-mock #'org-agenda-bulk-mark)
            (causalt-mock #'org-agenda-bulk-mark-regexp)
            (causalt-mock #'org-agenda-bulk-unmark)
            (causalt-mock #'org-agenda-bulk-unmark-all)
            (causalt-mock #'org-agenda-bulk-toggle)
            (causalt-mock #'org-agenda-bulk-toggle-all)
            (causalt-mock #'org-agenda-bulk-action)

            (causalt-mock #'org-agenda-previous-line)
            (causalt-mock #'org-agenda-next-line)
            (causalt-mock #'org-agenda-previous-item)
            (causalt-mock #'org-agenda-next-item)
            (causalt-mock #'org-agenda-previous-date-line)
            (causalt-mock #'org-agenda-next-date-line)
            (causalt-mock #'org-agenda-earlier)
            (causalt-mock #'org-agenda-later)
            (causalt-mock #'org-agenda-redo)
            (causalt-mock #'org-agenda-redo-all)
            (causalt-mock #'org-agenda-goto-date)
            (causalt-mock #'org-agenda-clock-goto))

    (let ((test-vectors
           '((:binding "m" :command org-agenda-bulk-mark)
             (:binding "x" :command org-agenda-bulk-mark-regexp)
             (:binding "u" :command org-agenda-bulk-unmark)
             (:binding "U" :command org-agenda-bulk-unmark-all)
             (:binding "t" :command org-agenda-bulk-toggle)
             (:binding "T" :command org-agenda-bulk-toggle-all)
             (:binding "B" :command org-agenda-bulk-action)

             (:binding "p" :command org-agenda-previous-line)
             (:binding "n" :command org-agenda-next-line)
             (:binding "P" :command org-agenda-previous-item)
             (:binding "N" :command org-agenda-next-item)
             (:binding "M-p" :command org-agenda-previous-date-line)
             (:binding "M-n" :command org-agenda-next-date-line)
             (:binding "b" :command org-agenda-earlier)
             (:binding "f" :command org-agenda-later)
             (:binding "r" :command org-agenda-redo)
             (:binding "g" :command org-agenda-redo-all)
             (:binding "j" :command org-agenda-goto-date)
             (:binding "M-j" :command org-agenda-clock-goto))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-agenda-mark-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-agenda-breakdown  t t))

(provide 'test-causal-agenda)
;;; test-causal-agenda.el ends here
