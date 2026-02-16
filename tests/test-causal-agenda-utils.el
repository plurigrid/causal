;;; test-causal-agenda-utils.el --- Causal Agenda Utils Tests  -*- lexical-binding: t; -*-

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
(require 'causal-agenda-test-utils)
(require 'causal-agenda-utils)

(ert-deftest test-causal-agenda-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal (causal-agenda-unicode-get :previous) "Prev"))
    (should (string-equal (causal-agenda-unicode-get :next) "Next"))
    (should (string-equal (causal-agenda-unicode-get :up) "Up"))
    (should (string-equal (causal-agenda-unicode-get :down) "Down"))
    (should (string-equal (causal-agenda-unicode-get :jumpdate) "Date"))
    (should (string-equal (causal-agenda-unicode-get :jumpclocked) "Clocked"))
    (should (string-equal (causal-agenda-unicode-get :jump) "Jump"))
    (should (string-equal (causal-agenda-unicode-get :date) "Date"))
    (should (string-equal (causal-agenda-unicode-get :heading) "*"))
    (should (string-equal (causal-agenda-unicode-get :timer) "Timer"))
    (should (string-equal (causal-agenda-unicode-get :sunrise) "Sunrise"))
    (should (string-equal (causal-agenda-unicode-get :lunar) "Lunar"))
    (should (string-equal (causal-agenda-unicode-get :jumpbookmark) "Jump to bookmark"))
    (should (string-equal (causal-agenda-unicode-get :clock) "Clock")))

  (let ((causal-lib-use-unicode t))
    (should (string-equal (causal-agenda-unicode-get :previous) "↑"))
    (should (string-equal (causal-agenda-unicode-get :next) "↓"))
    (should (string-equal (causal-agenda-unicode-get :up) "↑"))
    (should (string-equal (causal-agenda-unicode-get :down) "↓"))
    (should (string-equal (causal-agenda-unicode-get :jumpdate) "🚀 📅"))
    (should (string-equal (causal-agenda-unicode-get :jumpclocked) "🚀 ⏰"))
    (should (string-equal (causal-agenda-unicode-get :jump) "🚀"))
    (should (string-equal (causal-agenda-unicode-get :date) "📅"))
    (should (string-equal (causal-agenda-unicode-get :heading) "✲"))
    (should (string-equal (causal-agenda-unicode-get :timer) "⏱️"))
    (should (string-equal (causal-agenda-unicode-get :sunrise) "🌅"))
    (should (string-equal (causal-agenda-unicode-get :lunar) "🌙"))
    (should (string-equal (causal-agenda-unicode-get :jumpbookmark) "🚀 🔖"))
    (should (string-equal (causal-agenda-unicode-get :clock) "⏰"))))

(provide 'test-causal-agenda-utils)
;;; test-causal-agenda-utils.el ends here
