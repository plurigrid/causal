;;; test-causal-bibtex-settings.el --- Causal Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-bibtex-test-utils)
(require 'causal-bibtex-settings)

(ert-deftest test-causal-bibtex-settings-tmenu ()
  (let ()
    (cl-letf ((causalt-mock #'causal-bibtex--customize-dialect)
              (causalt-mock #'causal-bibtex--customize-file-path)
              (causalt-mock #'causal-bibtex--customize-files)
              (causalt-mock #'causal-bibtex--customize-search-entry-globally)
              (causalt-mock #'causal-bibtex--customize-clean-entry-hook)
              (causalt-mock #'causal-bibtex--customize-add-entry-hook)
              (causalt-mock #'causal-bibtex--customize-group)
              (causalt-mock #'causal-bibtex-about))

      (let ((test-vectors
             '((:binding "d" :command causal-bibtex--customize-dialect)
               (:binding "G" :command causal-bibtex--customize-group)
               (:binding "p" :command causal-bibtex--customize-file-path)
               (:binding "f" :command causal-bibtex--customize-files)
               (:binding "g" :command causal-bibtex--customize-search-entry-globally)
               (:binding "C" :command causal-bibtex--customize-clean-entry-hook)
               (:binding "A" :command causal-bibtex--customize-add-entry-hook)
               (:binding "u" :command causal-lib-customize-causal-lib-use-unicode)
               (:binding "n" :command causal-lib-customize-causal-lib-hide-navigation)
               (:binding "a" :command causal-bibtex-about))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-bibtex-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-causal-bibtex-about ()
  (should (stringp (causal-bibtex-about))))

(provide 'test-causal-bibtex-settings)
;;; test-causal-bibtex-settings.el ends here
