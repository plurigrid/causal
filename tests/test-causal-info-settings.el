;;; test-causal-info-settings.el --- Causal Info Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-info-test-utils)
(require 'causal-info-settings)

(ert-deftest test-causal-info-settings-tmenu ()
  (let ()
    (causalt-info-setup)
    (cl-letf ((causalt-mock #'causal-info--customize-info-group)
              (causalt-mock #'causal-info--customize-info-hide-note-references)
              (causalt-mock #'causal-info--customize-info-additional-directory-list)
              (causalt-mock #'causal-info--customize-info-isearch-search)
              (causalt-mock #'causal-info--customize-info-scroll-prefer-subnodes)
              (causalt-mock #'causal-info-about))

      (let ((test-vectors
             '((:binding "h" :command causal-info--customize-info-hide-note-references)
               (:binding "A" :command causal-info--customize-info-additional-directory-list)
               (:binding "i" :command causal-info--customize-info-isearch-search)
               (:binding "c" :command causal-info--customize-info-scroll-prefer-subnodes)
               (:binding "G" :command causal-info--customize-info-group)
               (:binding "a" :command causal-info-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-info-settings-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-info-breakdown)))

(provide 'test-causal-info-settings)
;;; test-causal-info-setttings.el ends here
