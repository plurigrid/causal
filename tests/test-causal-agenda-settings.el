;;; test-causal-agenda-settings.el --- Causal Agenda Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-agenda-settings)

(ert-deftest test-causal-agenda-about ()
  (should (stringp (causal-agenda-about))))

(ert-deftest test-causal-agenda-settings-tmenu ()
  (causalt-agenda-setup)
  (cl-letf (((symbol-function #'causal-agenda-headlinep) (lambda () t))
            (causalt-mock #'org-agenda-log-mode)
            (causalt-mock #'org-agenda-toggle-time-grid)
            (causalt-mock #'org-agenda-toggle-diary)
            (causalt-mock #'org-agenda-follow-mode)
            (causalt-mock #'org-agenda-clockreport-mode)
            (causalt-mock #'org-agenda-entry-text-mode))

    (let ((test-vectors
           '((:binding "l" :command org-agenda-log-mode)
             (:binding "G" :command org-agenda-toggle-time-grid)
             (:binding "D" :command org-agenda-toggle-diary)
             (:binding "F" :command org-agenda-follow-mode)
             (:binding "R" :command org-agenda-clockreport-mode)
             (:binding "E" :command org-agenda-entry-text-mode)

             (:binding "f" :command causal-agenda-customize-agenda-files)
             (:binding "d" :command causal-agenda-customize-agenda-include-diary)
             (:binding "i" :command causal-agenda-customize-agenda-include-inactive-timestamps)
             (:binding "x" :command causal-agenda-customize-agenda-include-deadlines)
             (:binding "m" :command causal-agenda-customize-ampm)
             (:binding "I" :command causal-agenda-customize-org-clock-in-switch-to-state)
             (:binding "O" :command causal-agenda-customize-org-clock-out-switch-to-state)
             (:binding "," :command causal-agenda--customize-group))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-agenda-settings-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-agenda-breakdown t))

(provide 'test-causal-agenda-settings)
;;; test-causal-agenda-setttings.el ends here
