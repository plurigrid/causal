;;; test-causal-calendar.el --- Causal Calendar Tests -*- lexical-binding: t; -*-

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
(require 'causal-lib-test-utils)
(require 'causal-calendar-test-utils)
(require 'causal-calendar)

(ert-deftest test-causal-calendar-tmenu ()
  (let ()
    (causalt-calendar-setup)

    (cl-letf (;; ((symbol-function #'buffer-file-name) (lambda () t))
              (causalt-mock #'calendar-backward-day)
              (causalt-mock #'calendar-forward-day)
              (causalt-mock #'calendar-goto-today)
              (causalt-mock #'calendar-goto-date)

              (causalt-mock #'calendar-backward-week)
              (causalt-mock #'calendar-forward-week)
              (causalt-mock #'calendar-beginning-of-week)
              (causalt-mock #'calendar-end-of-week)
              (causalt-mock #'calendar-iso-goto-week)
              (causalt-mock #'calendar-iso-print-date)

              (causalt-mock #'calendar-backward-month)
              (causalt-mock #'calendar-forward-month)
              (causalt-mock #'calendar-beginning-of-month)
              (causalt-mock #'calendar-end-of-month)
              (causalt-mock #'calendar-other-month)

              (causalt-mock #'calendar-backward-year)
              (causalt-mock #'calendar-forward-year)
              (causalt-mock #'calendar-beginning-of-year)
              (causalt-mock #'calendar-end-of-year)

              (causalt-mock #'calendar-scroll-right)
              (causalt-mock #'calendar-scroll-left)
              (causalt-mock #'calendar-scroll-right-three-months)
              (causalt-mock #'calendar-scroll-left-three-months)
              (causalt-mock #'calendar-redraw)

              (causalt-mock #'calendar-print-other-dates)
              (causalt-mock #'calendar-list-holidays)
              (causalt-mock #'calendar-cursor-holidays)
              (causalt-mock #'calendar-mark-holidays)
              (causalt-mock #'calendar-unmark)
              (causalt-mock #'org-calendar-goto-agenda)
              (causalt-mock #'diary-view-entries)
              (causalt-mock #'diary-show-all-entries)
              (causalt-mock #'calendar-lunar-phases)
              (causalt-mock #'calendar-sunrise-sunset)
              (causalt-mock #'calendar-sunrise-sunset-month)
              (causalt-mock #'calendar-set-mark)
              (causalt-mock #'calendar-count-days-region)

              (causalt-mock #'calendar-exit))

      (let ((test-vectors
             '((:binding "b" :command calendar-backward-day)
               (:binding "f" :command calendar-forward-day)
               (:binding "." :command calendar-goto-today)
               (:binding "g" :command calendar-goto-date)

               (:binding "p" :command calendar-backward-week)
               (:binding "n" :command calendar-forward-week)
               (:binding "a" :command calendar-beginning-of-week)
               (:binding "e" :command calendar-end-of-week)
               ;;(:binding "w" :command calendar-iso-goto-week) ; TODO: needs input handling

               (:binding "{" :command calendar-backward-month)
               (:binding "}" :command calendar-forward-month)
               (:binding "M-a" :command calendar-beginning-of-month)
               (:binding "M-e" :command calendar-end-of-month)
               (:binding "o" :command calendar-other-month)

               (:binding "M-]" :command calendar-forward-year)
               (:binding "M-[" :command calendar-backward-year)
               (:binding "[" :command calendar-beginning-of-year)
               (:binding "]" :command calendar-end-of-year)

               (:binding "<" :command calendar-scroll-right)
               (:binding ">" :command calendar-scroll-left)
               (:binding "-" :command calendar-scroll-right-three-months)
               (:binding "+" :command calendar-scroll-left-three-months)
               (:binding "C-l" :command calendar-redraw)

               (:binding "c" :command causal-calendar-conversions-tmenu)
               (:binding "A" :command calendar-print-other-dates)
               (:binding "i" :command calendar-iso-print-date)
               (:binding "H" :command calendar-list-holidays)
               (:binding "h" :command calendar-cursor-holidays)
               (:binding "x" :command calendar-mark-holidays)
               (:binding "u" :command calendar-unmark)
               (:binding "O" :command org-calendar-goto-agenda)
               (:binding "d" :command diary-view-entries)
               (:binding "D" :command causal-calendar-diary-and-goto-tmenu)
               (:binding "s" :command diary-show-all-entries)
               (:binding "M" :command calendar-lunar-phases)
               (:binding "S" :command calendar-sunrise-sunset)
               (:binding "M-m" :command calendar-sunrise-sunset-month)
               (:binding "C-SPC" :command calendar-set-mark)
               (:binding "=" :command calendar-count-days-region))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(provide 'test-causal-calendar)
;;; test-causal-calendar.el ends here
