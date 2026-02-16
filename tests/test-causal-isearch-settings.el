;;; test-causal-isearch-settings.el --- Causal Re-Builder Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-isearch-test-utils)
(require 'causal-isearch-settings)

(ert-deftest test-causal-isearch-settings-tmenu ()
  (let ()
    (causalt-setup)
    (cl-letf ((causalt-mock #'causal-isearch--customize-group)
              (causalt-mock #'causal-isearch--customize-allow-scroll)
              (causalt-mock #'causal-isearch--customize-allow-motion)
              (causalt-mock #'causal-isearch--customize-lazy-count)
              (causalt-mock #'causal-isearch--customize-lazy-highlight))

      (let ((test-vectors
             '((:binding "s" :command causal-isearch--customize-allow-scroll)
               (:binding "m" :command causal-isearch--customize-allow-motion)
               (:binding "h" :command causal-isearch--customize-lazy-highlight)
               (:binding "c" :command causal-isearch--customize-lazy-count)
               (:binding "G" :command causal-isearch--customize-group)
               (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
               (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
               (:binding "a" :command causal-isearch-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-isearch-settings-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-breakdown)))

(ert-deftest test-causal-isearch-about ()
  (should (stringp (causal-isearch-about))))

(provide 'test-causal-isearch-settings)
;;; test-causal-isearch-setttings.el ends here
