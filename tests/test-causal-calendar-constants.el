(require 'ert);;; test-causal-calendar-constants.el --- Causal Calendar Tests -*- lexical-binding: t; -*-

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


(require 'causal-lib-test-utils)
(require 'causal-calendar-test-utils)
(require 'causal-calendar)

(defun causalt-calendar-unicode-assert (key control)
 (causalt-unicode-db-assert key control #'causal-calendar-unicode-get))

(ert-deftest test-causal-calendar-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (causalt-calendar-unicode-assert :previous "Previous")
    (causalt-calendar-unicode-assert :next "Next")
    (causalt-calendar-unicode-assert :behind "Behind")
    (causalt-calendar-unicode-assert :ahead "Ahead")
    (causalt-calendar-unicode-assert :beginning "Beginning")
    (causalt-calendar-unicode-assert :end "End")
    (causalt-calendar-unicode-assert :back-3-months "-3 months")
    (causalt-calendar-unicode-assert :forward-3-months "+3 months")
    (causalt-calendar-unicode-assert :redraw "Refresh")
    (causalt-calendar-unicode-assert :goto "Goto")
    (causalt-calendar-unicode-assert :sunrise "Sunrise")
    (causalt-calendar-unicode-assert :lunar "Lunar"))

  (let ((causal-lib-use-unicode t))
    (causalt-calendar-unicode-assert :previous "↑")
    (causalt-calendar-unicode-assert :next "↓")
    (causalt-calendar-unicode-assert :behind "←")
    (causalt-calendar-unicode-assert :ahead "→")
    (causalt-calendar-unicode-assert :beginning "⇤")
    (causalt-calendar-unicode-assert :end "⇥")
    (causalt-calendar-unicode-assert :back-3-months "← 3 months")
    (causalt-calendar-unicode-assert :forward-3-months "→ 3 months")
    (causalt-calendar-unicode-assert :redraw "⟳")
    (causalt-calendar-unicode-assert :goto "🔎")
    (causalt-calendar-unicode-assert :sunrise "🌅")
    (causalt-calendar-unicode-assert :lunar "🌙")))

(provide 'test-causal-calendar-constants)
;;; test-causal-calendar-constants.el ends here
