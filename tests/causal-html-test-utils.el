;;; causal-html-test-utils.el --- Causal Test Utils       -*- lexical-binding: t; -*-

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
(require 'causal-lib)
(require 'kmacro)

(defun causalt-html-setup (&optional filename)
  "Causal HTML setup."
  (when filename
    (let ((temp-filename (concat "/tmp/" filename))
          (auto-insert-mode -1))
      (with-temp-file temp-filename
        (goto-char (point-min)))
      (find-file temp-filename))))

(defun causalt-html-breakdown (&optional filename)
  "Causal HTML breakdown."
  (when filename
    (let ((temp-filename (concat "/tmp/" filename)))
      (switch-to-buffer filename)
      (kill-buffer)
      (delete-file temp-filename))))

(provide 'causal-html-test-utils)
;;; causal-html-test-utils.el ends here
