;;; test-causal-org-settings.el --- Causal Make Settings Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Charles Y. Choi

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
(require 'causal-org-test-utils)
(require 'causal-org-settings)

(ert-deftest test-causal-org-settings-tmenu ()
  (cl-letf ((causalt-mock #'causal-org-settings-heading-tmenu)
            (causalt-mock #'causal-org-settings-files-tmenu)
            (causalt-mock #'causal-org-settings-display-tmenu)
            (causalt-mock #'causal-org-settings-keyboard-tmenu)
            (causalt-mock #'causal-org-settings-clock-tmenu)
            (causalt-mock #'causal-org-settings-killyank-tmenu)
            (causalt-mock #'causal-org--customize-group)
            (causalt-mock #'causal-org-about))

    (let ((test-vectors
           '((:binding "h" :command causal-org-settings-heading-tmenu)
             (:binding "f" :command causal-org-settings-files-tmenu)
             (:binding "d" :command causal-org-settings-display-tmenu)
             (:binding "k" :command causal-org-settings-keyboard-tmenu)
             (:binding "c" :command causal-org-settings-clock-tmenu)
             (:binding "y" :command causal-org-settings-killyank-tmenu)
             (:binding "G" :command causal-org--customize-group)
             (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
             (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
             (:binding "a" :command causal-org-about))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-settings-tmenu
                                      '(lambda () (random 5000))))))


(ert-deftest test-causal-org-settings-heading-tmenu ()
  (cl-letf (

            (causalt-mock #'causal-org--customize-todo-keywords)
            (causalt-mock #'causal-org--customize-log-done)
            (causalt-mock #'causal-org--customize-group-priorities)
            (causalt-mock #'causal-org--customize-imenu-depth)
            (causalt-mock #'causal-org--customize-insert-heading-respect-content)
            (causalt-mock #'causal-org--customize-group-goto)

            )

    (let ((test-vectors
           '(
            (:binding "k" :command causal-org--customize-todo-keywords)
            (:binding "l" :command causal-org--customize-log-done)
            (:binding "p" :command causal-org--customize-group-priorities)
            (:binding "i" :command causal-org--customize-imenu-depth)
            (:binding "r" :command causal-org--customize-insert-heading-respect-content)
            (:binding "g" :command causal-org--customize-group-goto)

             )))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-settings-heading-tmenu
                                      '(lambda () (random 5000))))))


(ert-deftest test-causal-org-settings-files-tmenu ()
  (cl-letf ((causalt-mock #'causal-org--customize-directory)
            (causalt-mock #'causal-org--customize-default-notes-file)
            (causalt-mock #'causal-org--customize-refile-targets))

    (let ((test-vectors
           '((:binding "o" :command causal-org--customize-directory)
             (:binding "n" :command causal-org--customize-default-notes-file)
             (:binding "r" :command causal-org--customize-refile-targets))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-settings-files-tmenu
                                      '(lambda () (random 5000))))))


(ert-deftest test-causal-org-settings-display-tmenu ()
  (cl-letf ((causalt-mock #'causal-org--customize-hide-leading-stars)
            (causalt-mock #'causal-org--customize-hide-emphasis-markers)
            (causalt-mock #'causal-org--customize-startup-folded)
            (causalt-mock #'causal-org--customize-startup-indented))

    (let ((test-vectors
           '((:binding "s" :command causal-org--customize-hide-leading-stars)
             (:binding "e" :command causal-org--customize-hide-emphasis-markers)
             (:binding "f" :command causal-org--customize-startup-folded)
             (:binding "i" :command causal-org--customize-startup-indented))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-settings-display-tmenu
                                      '(lambda () (random 5000))))))


(ert-deftest test-causal-org-settings-keyboard-tmenu ()
  (cl-letf ((causalt-mock #'causal-org--customize-support-shift-select)
            (causalt-mock #'causal-org--customize-use-speed-commands))

    (let ((test-vectors
           '((:binding "s" :command causal-org--customize-support-shift-select)
             (:binding "S" :command causal-org--customize-use-speed-commands))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-settings-keyboard-tmenu
                                      '(lambda () (random 5000))))))

(ert-deftest test-causal-org-settings-clock-tmenu ()
  (cl-letf ((causalt-mock #'causal-org--customize-show-notification-handler))

    (let ((test-vectors
           '((:binding "n"
              :command causal-org--customize-show-notification-handler))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-settings-clock-tmenu
                                      '(lambda () (random 5000))))))

(ert-deftest test-causal-org-settings-killyank-tmenu ()
  (cl-letf ((causalt-mock #'causal-org--customize-yank-image-save-method))

    (let ((test-vectors
           '((:binding "y"
              :command causal-org--customize-yank-image-save-method))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-settings-killyank-tmenu
                                      '(lambda () (random 5000))))))

(ert-deftest test-causal-org-about ()
  (should (stringp (causal-org-about))))

(provide 'test-causal-org-settings)
;;; test-causal-org-setttings.el ends here
