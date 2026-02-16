;;; test-causal-calendar-settings.el --- Causal Calendar Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-calendar-settings)

(ert-deftest test-causal-calendar-settings-tmenu ()
  (let ()
    (causalt-calendar-setup)
    (cl-letf ()
      (let ((test-vectors
             '((:binding "C" :command causal-calendar--customize-calendar-group)
               (:binding "H" :command causal-calendar--customize-calendar-mark-holidays-flag)
               (:binding "E" :command causal-calendar--customize-calendar-mark-diary-entries-flag)
               (:binding "v" :command causal-calendar--customize-calendar-move-hook)
               (:binding "N" :command causal-calendar--customize-calendar-location-name)
               (:binding "A" :command causal-calendar--customize-calendar-latitude)
               (:binding "O" :command causal-calendar--customize-calendar-longitude)
               (:binding "D" :command causal-calendar--customize-diary-group)
               (:binding "l" :command causal-calendar--customize-diary-list-entries-hook)
               (:binding "m" :command causal-calendar--customize-diary-mark-entries-hook)
               (:binding "L" :command causal-calendar--customize-diary-nongregorian-listing-hook)
               (:binding "M" :command causal-calendar--customize-diary-nongregorian-marking-hook)
               (:binding "d" :command causal-calendar--customize-org-agenda-include-diary)
               (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
               (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
               (:binding "a" :command causal-calendar-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-calendar-settings-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-calendar-breakdown)))

(ert-deftest test-causal-calendar-about ()
  (should (stringp (causal-calendar-about))))

(provide 'test-causal-calendar-settings)
;;; test-causal-calendar-settings.el ends here
