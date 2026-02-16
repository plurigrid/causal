;;; test-causal-editkit-constants.el --- Tests for causal-editkit-constants  -*- lexical-binding: t; -*-

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
(require 'causal-editkit-test-utils)
(require 'causal-editkit-utils)

(defun causalt-unicode-db-assert (key control cmd)
  (let ((test (funcall cmd key)))
    (should (string= test control))))

(defun causalt-editkit-unicode-assert (key control)
  (causalt-unicode-db-assert key control #'causal-editkit-unicode-get))

(ert-deftest test-causal-editkit-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (causalt-editkit-unicode-assert :previous "previous")
    (causalt-editkit-unicode-assert :next "next")
    (causalt-editkit-unicode-assert :point-up "Up")
    (causalt-editkit-unicode-assert :point-down "Down")
    (causalt-editkit-unicode-assert :point-left "Left")
    (causalt-editkit-unicode-assert :point-right "Right")
    (causalt-editkit-unicode-assert :window-above "Above")
    (causalt-editkit-unicode-assert :window-below "Below")
    (causalt-editkit-unicode-assert :window-left "To Left")
    (causalt-editkit-unicode-assert :window-right "To Right")
    (causalt-editkit-unicode-assert :other-window "Other")
    (causalt-editkit-unicode-assert :delete-other-windows "Delete other")
    (causalt-editkit-unicode-assert :split-window-below "Below")
    (causalt-editkit-unicode-assert :split-window-horizontally "Right")
    (causalt-editkit-unicode-assert :enlarge "Enlarge")
    (causalt-editkit-unicode-assert :shrink "Shrink")
    (causalt-editkit-unicode-assert :horizontal "Horizontal")
    (causalt-editkit-unicode-assert :vertical "Vertical")
    (causalt-editkit-unicode-assert :first "first")
    (causalt-editkit-unicode-assert :last "last")
    (causalt-editkit-unicode-assert :swap "Swap")
    (causalt-editkit-unicode-assert :jump "Jump"))

  (let ((causal-lib-use-unicode t))
    (causalt-editkit-unicode-assert :previous "↑")
    (causalt-editkit-unicode-assert :next "↓")
    (causalt-editkit-unicode-assert :point-up "↑")
    (causalt-editkit-unicode-assert :point-down "↓")
    (causalt-editkit-unicode-assert :point-left "←")
    (causalt-editkit-unicode-assert :point-right "→")
    (causalt-editkit-unicode-assert :window-above "↑")
    (causalt-editkit-unicode-assert :window-below "↓")
    (causalt-editkit-unicode-assert :window-left "←")
    (causalt-editkit-unicode-assert :window-right "→")
    (causalt-editkit-unicode-assert :other-window "»")
    (causalt-editkit-unicode-assert :delete-other-windows "❏")
    (causalt-editkit-unicode-assert :split-window-below "━")
    (causalt-editkit-unicode-assert :split-window-horizontally "┃")
    (causalt-editkit-unicode-assert :enlarge "+")
    (causalt-editkit-unicode-assert :shrink "−")
    (causalt-editkit-unicode-assert :horizontal "↔︎")
    (causalt-editkit-unicode-assert :vertical "↕︎")
    (causalt-editkit-unicode-assert :first "⤒")
    (causalt-editkit-unicode-assert :last "⤓")
    (causalt-editkit-unicode-assert :swap "⇄")
    (causalt-editkit-unicode-assert :jump "🚀")))

(provide 'test-causal-editkit-constants)
;;; test-causal-editkit-constants.el ends here
