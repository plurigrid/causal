;;; test-causal-bookmarks-settings.el --- Causal IBuffer Settings Tests  -*- lexical-binding: t; -*-

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
(require 'causal-bookmarks-test-utils)
(require 'causal-bookmarks-settings)

(ert-deftest test-causal-bookmarks-settings-tmenu-bindings ()
  (causalt-bookmarks-setup)
  (let
      ((test-vectors
        (list
         (causalt-suffix-test-vector "G" #'causal-bookmarks--customize-group)
         (causalt-suffix-test-vector "u" #'causal-lib-customize-causal-lib-use-unicode)
         (causalt-suffix-test-vector "n" #'causal-lib-customize-causal-lib-hide-navigation)
         (causalt-suffix-test-vector "a" #'causal-bookmarks-about)
 )))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-bookmarks-settings-tmenu
                                     '(lambda () (random 5000))))
  (causalt-bookmarks-breakdown t))

(ert-deftest test-causal-bookmarks-about ()
  (should (stringp (causal-bookmarks-about))))

(provide 'test-causal-bookmarks-settings)
;;; test-causal-bookmarks-setttings.el ends here
