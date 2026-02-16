;;; test-causal-calendar-utils.el --- Causal Calendar Utils Tests  -*- lexical-binding: t; -*-

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
(require 'causal-calendar-test-utils)
(require 'causal-calendar-utils)

(ert-deftest test-causal-calendar-diary-and-goto-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ((causalt-mock #'diary-insert-entry)
              (causalt-mock #'diary-insert-weekly-entry)
              (causalt-mock #'diary-insert-monthly-entry)
              (causalt-mock #'diary-insert-yearly-entry)
              (causalt-mock #'diary-insert-anniversary-entry)
              (causalt-mock #'calendar-goto-date)
              (causalt-mock #'calendar-iso-goto-date)
              (causalt-mock #'calendar-goto-day-of-year))

      (let ((test-vectors
             '((:binding "e" :command diary-insert-entry)
               (:binding "w" :command diary-insert-weekly-entry)
               (:binding "m" :command diary-insert-monthly-entry)
               (:binding "y" :command diary-insert-yearly-entry)
               (:binding "a" :command diary-insert-anniversary-entry)
               (:binding "g" :command calendar-goto-date)
               ;; (:binding "i" :command calendar-iso-goto-date) ; TODO: need to mock input
               (:binding "d" :command calendar-goto-day-of-year))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-diary-and-goto-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown t)))

(ert-deftest test-causal-calendar-conversions-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ((causalt-mock #'calendar-print-other-dates))

      (let ((test-vectors
             '((:binding "a" :command causal-calendar-astro-tmenu)
               (:binding "b" :command causal-calendar-bahai-tmenu)
               (:binding "c" :command causal-calendar-coptic-tmenu)
               (:binding "e" :command causal-calendar-ethiopic-tmenu)
               (:binding "f" :command causal-calendar-french-tmenu)
               (:binding "h" :command causal-calendar-hebrew-tmenu)
               (:binding "i" :command causal-calendar-islamic-tmenu)
               (:binding "j" :command causal-calendar-julian-tmenu)
               (:binding "l" :command causal-calendar-lunar-tmenu)
               (:binding "m" :command causal-calendar-mayan-tmenu)
               (:binding "p" :command causal-calendar-persian-tmenu)
               (:binding "A" :command calendar-print-other-dates))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-conversions-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(ert-deftest test-causal-calendar-lunar-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ((causalt-mock #'calendar-chinese-print-date)
              (causalt-mock #'calendar-chinese-goto-date)
              (causalt-mock #'diary-chinese-insert-entry)
              (causalt-mock #'diary-chinese-insert-monthly-entry)
              (causalt-mock #'diary-chinese-insert-yearly-entry)
              (causalt-mock #'diary-chinese-insert-anniversary-entry)
              (causalt-mock #'diary-view-entries)
              (causalt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-chinese-print-date)
               ;;(:binding "G" :command calendar-chinese-goto-date)
               (:binding "i" :command diary-chinese-insert-entry)
               (:binding "m" :command diary-chinese-insert-monthly-entry)
               (:binding "y" :command diary-chinese-insert-yearly-entry)
               (:binding "A" :command diary-chinese-insert-anniversary-entry)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-lunar-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(ert-deftest test-causal-calendar-astro-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ((causalt-mock #'calendar-astro-print-day-number)
              (causalt-mock #'calendar-astro-goto-day-number)
              (causalt-mock #'diary-view-entries)
              (causalt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-astro-print-day-number)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-astro-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(ert-deftest test-causal-calendar-islamic-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ((causalt-mock #'calendar-islamic-print-day-number)
              (causalt-mock #'calendar-islamic-goto-day-number)
              (causalt-mock #'diary-islamic-insert-entry)
              (causalt-mock #'diary-islamic-insert-monthly-entry)
              (causalt-mock #'diary-islamic-insert-yearly-entry)
              (causalt-mock #'diary-view-entries)
              (causalt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-islamic-print-date)
               ;;(:binding "G" :command calendar-islamic-goto-date)
               (:binding "i" :command diary-islamic-insert-entry)
               (:binding "m" :command diary-islamic-insert-monthly-entry)
               (:binding "y" :command diary-islamic-insert-yearly-entry)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-islamic-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(ert-deftest test-causal-calendar-hebrew-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ((causalt-mock #'calendar-hebrew-print-day-number)
              (causalt-mock #'calendar-hebrew-goto-day-number)
              (causalt-mock #'diary-hebrew-insert-entry)
              (causalt-mock #'diary-hebrew-insert-monthly-entry)
              (causalt-mock #'diary-hebrew-insert-yearly-entry)
              (causalt-mock #'diary-view-entries)
              (causalt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-hebrew-print-date)
               ;;(:binding "G" :command calendar-hebrew-goto-date)
               (:binding "i" :command diary-hebrew-insert-entry)
               (:binding "m" :command diary-hebrew-insert-monthly-entry)
               (:binding "y" :command diary-hebrew-insert-yearly-entry)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-hebrew-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(ert-deftest test-causal-calendar-bahai-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ((causalt-mock #'calendar-bahai-print-day-number)
              (causalt-mock #'calendar-bahai-goto-day-number)
              (causalt-mock #'diary-bahai-insert-entry)
              (causalt-mock #'diary-bahai-insert-monthly-entry)
              (causalt-mock #'diary-bahai-insert-yearly-entry)
              (causalt-mock #'diary-view-entries)
              (causalt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-bahai-print-date)
               ;;(:binding "G" :command calendar-bahai-goto-date)
               (:binding "i" :command diary-bahai-insert-entry)
               (:binding "m" :command diary-bahai-insert-monthly-entry)
               (:binding "y" :command diary-bahai-insert-yearly-entry)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-bahai-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(ert-deftest test-causal-calendar-ethiopic-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ((causalt-mock #'calendar-ethiopic-print-day-number)
              (causalt-mock #'calendar-ethiopic-goto-day-number)
              (causalt-mock #'diary-view-entries)
              (causalt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-ethiopic-print-date)
               ;;(:binding "G" :command calendar-ethiopic-goto-date)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-ethiopic-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(ert-deftest test-causal-calendar-french-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ((causalt-mock #'calendar-french-print-day-number)
              (causalt-mock #'calendar-french-goto-day-number)
              (causalt-mock #'diary-view-entries)
              (causalt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-french-print-date)
               ;;(:binding "G" :command calendar-french-goto-date)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-french-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(ert-deftest test-causal-calendar-julian-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ((causalt-mock #'calendar-julian-print-day-number)
              (causalt-mock #'calendar-julian-goto-day-number)
              (causalt-mock #'diary-view-entries)
              (causalt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-julian-print-date)
               ;;(:binding "G" :command calendar-julian-goto-date)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-julian-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(ert-deftest test-causal-calendar-coptic-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ((causalt-mock #'calendar-coptic-print-day-number)
              (causalt-mock #'calendar-coptic-goto-day-number)
              (causalt-mock #'diary-view-entries)
              (causalt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-coptic-print-date)
               ;;(:binding "G" :command calendar-coptic-goto-date)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-coptic-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(ert-deftest test-causal-calendar-persian-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ((causalt-mock #'calendar-persian-print-day-number)
              (causalt-mock #'calendar-persian-goto-day-number)
              (causalt-mock #'diary-view-entries)
              (causalt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-persian-print-date)
               ;;(:binding "G" :command calendar-persian-goto-date)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-persian-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(ert-deftest test-causal-calendar-mayan-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ((causalt-mock #'calendar-mayan-print-day-number)
              (causalt-mock #'calendar-mayan-goto-day-number)
              (causalt-mock #'diary-view-entries)
              (causalt-mock #'diary-show-all-entries))

      (let ((test-vectors
             '((:binding "c" :command calendar-mayan-print-date)
               ;;(:binding "G" :command calendar-mayan-goto-date)
               (:binding "d" :command diary-view-entries)
               (:binding "s" :command diary-show-all-entries))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-mayan-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(provide 'test-causal-calendar-utils)
;;; test-causal-calendar-utils.el ends here
