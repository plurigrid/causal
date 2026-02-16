;;; test-causal-bookmarks-utils.el --- Causal IBuffer Utils Tests  -*- lexical-binding: t; -*-

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
(require 'causal-bookmarks-utils)

(defun causalt-unicode-db-assert (key control cmd)
  (let ((test (funcall cmd key)))
    (should (string= test control))))

(defun causalt-bookmarks-unicode-assert (key control)
  (causalt-unicode-db-assert key control #'causal-bookmarks-unicode-get))

(ert-deftest test-causal-bookmarks-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (causalt-bookmarks-unicode-assert :previous "Previous")
    (causalt-bookmarks-unicode-assert :next "Next")
    (causalt-bookmarks-unicode-assert :beginning-of-buffer "Beginning")
    (causalt-bookmarks-unicode-assert :end-of-buffer "End")
    (causalt-bookmarks-unicode-assert :backward "Backward")
    (causalt-bookmarks-unicode-assert :forward "Forward")
    (causalt-bookmarks-unicode-assert :narrow "Narrow")
    (causalt-bookmarks-unicode-assert :widen "Widen")
    (causalt-bookmarks-unicode-assert :jump "Jump"))

  (let ((causal-lib-use-unicode t))
    (causalt-bookmarks-unicode-assert :previous "↑")
    (causalt-bookmarks-unicode-assert :next "↓")
    (causalt-bookmarks-unicode-assert :beginning-of-buffer "⤒")
    (causalt-bookmarks-unicode-assert :end-of-buffer "⤓")
    (causalt-bookmarks-unicode-assert :backward "←")
    (causalt-bookmarks-unicode-assert :forward "→")
    (causalt-bookmarks-unicode-assert :narrow "→←")
    (causalt-bookmarks-unicode-assert :widen "←→")
    (causalt-bookmarks-unicode-assert :jump "🚀")))

(provide 'test-causal-bookmarks-utils)
;;; test-causal-bookmarks-utils.el ends here
