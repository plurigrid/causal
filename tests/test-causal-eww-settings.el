;;; test-causal-eww-settings.el --- Causal Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-eww-test-utils)
(require 'causal-eww-settings)

(ert-deftest test-causal-eww-settings-tmenu ()
  (let ()
    (cl-letf ((causalt-mock #'causal-eww--customize-retrieve-command)
              (causalt-mock #'causal-eww--customize-readable-urls)
              (causalt-mock #'causal-eww--customize-history-limit)
              (causalt-mock #'causal-eww--customize-download-directory)
              (causalt-mock #'causal-eww--customize-bookmarks-directory)
              (causalt-mock #'causal-eww--customize-shr-use-fonts)
              (causalt-mock #'causal-eww--customize-shr-use-colors)
              (causalt-mock #'causal-eww--customize-shr-inhibit-images)
              (causalt-mock #'causal-eww--customize-group)
              (causalt-mock #'causal-eww-about))

      (let ((test-vectors
             '((:binding "r" :command causal-eww--customize-retrieve-command)
               (:binding "R" :command causal-eww--customize-readable-urls)
               (:binding "h" :command causal-eww--customize-history-limit)
               (:binding "d" :command causal-eww--customize-download-directory)
               (:binding "b" :command causal-eww--customize-bookmarks-directory)
               (:binding "f" :command causal-eww--customize-shr-use-fonts)
               (:binding "c" :command causal-eww--customize-shr-use-colors)
               (:binding "i" :command causal-eww--customize-shr-inhibit-images)
               (:binding "G" :command causal-eww--customize-group)
               (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
               (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
               (:binding "a" :command causal-eww-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-eww-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-causal-eww-about ()
  (should (stringp (causal-eww-about))))

(provide 'test-causal-eww-settings)
;;; test-causal-eww-setttings.el ends here
