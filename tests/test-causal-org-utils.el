;;; test-causal-org-utils.el --- Causal Make Utils Tests  -*- lexical-binding: t; -*-

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
(require 'causal-org-test-utils)
(require 'causal-org-utils)

(ert-deftest test-causal-org-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal (causal-org-unicode-get :previous) "Previous"))
    (should (string-equal (causal-org-unicode-get :next) "Next"))
    (should (string-equal (causal-org-unicode-get :cycle) "Cycle"))
    (should (string-equal (causal-org-unicode-get :shift-cycle) "S-Cycle"))
    (should (string-equal (causal-org-unicode-get :up) "Up"))
    (should (string-equal (causal-org-unicode-get :down) "Down"))
    (should (string-equal (causal-org-unicode-get :left) "Left"))
    (should (string-equal (causal-org-unicode-get :right) "Right"))
    (should (string-equal (causal-org-unicode-get :beginning-of-line) "BoL"))
    (should (string-equal (causal-org-unicode-get :end-of-line) "EoL"))
    (should (string-equal (causal-org-unicode-get :beginning-of-line-table) "Begin"))
    (should (string-equal (causal-org-unicode-get :end-of-line-table) "End"))
    (should (string-equal (causal-org-unicode-get :beginning-of-field) "Begin"))
    (should (string-equal (causal-org-unicode-get :end-of-field) "End"))
    (should (string-equal (causal-org-unicode-get :first-row) "First"))
    (should (string-equal (causal-org-unicode-get :last-row) "Last"))
    (should (string-equal (causal-org-unicode-get :first-column) "First"))
    (should (string-equal (causal-org-unicode-get :last-column) "Last"))
    (should (string-equal (causal-org-unicode-get :row) "Row"))
    (should (string-equal (causal-org-unicode-get :column) "Column"))
    (should (string-equal (causal-org-unicode-get :beginning-of-buffer) "Beginning"))
    (should (string-equal (causal-org-unicode-get :end-of-buffer) "End"))
    (should (string-equal (causal-org-unicode-get :info-functions) "Info f(x)"))
    (should (string-equal (causal-org-unicode-get :info) "Info"))
    (should (string-equal (causal-org-unicode-get :clock-in) "In"))
    (should (string-equal (causal-org-unicode-get :clock-out) "Out"))
    (should (string-equal (causal-org-unicode-get :clock-report) "Report"))
    (should (string-equal (causal-org-unicode-get :paragraph) "Paragraph"))
    (should (string-equal (causal-org-unicode-get :update) "Update"))
    (should (string-equal (causal-org-unicode-get :kill) "Close")))

  (let ((causal-lib-use-unicode t))
    (should (string-equal (causal-org-unicode-get :previous) "↑"))
    (should (string-equal (causal-org-unicode-get :next) "↓"))
    (should (string-equal (causal-org-unicode-get :cycle) "⥅"))
    (should (string-equal (causal-org-unicode-get :shift-cycle) "⥆"))
    (should (string-equal (causal-org-unicode-get :up) "↑"))
    (should (string-equal (causal-org-unicode-get :down) "↓"))
    (should (string-equal (causal-org-unicode-get :left) "←"))
    (should (string-equal (causal-org-unicode-get :right) "→"))
    (should (string-equal (causal-org-unicode-get :beginning-of-line) "⇤"))
    (should (string-equal (causal-org-unicode-get :end-of-line) "⇥"))
    (should (string-equal (causal-org-unicode-get :beginning-of-line-table) "⇤"))
    (should (string-equal (causal-org-unicode-get :beginning-of-field) "⇤"))
    (should (string-equal (causal-org-unicode-get :end-of-line-table) "⇥"))
    (should (string-equal (causal-org-unicode-get :end-of-field) "⇥"))
    (should (string-equal (causal-org-unicode-get :first-row) "⤒ First"))
    (should (string-equal (causal-org-unicode-get :last-row) "⤓ Last"))
    (should (string-equal (causal-org-unicode-get :first-column) "⇤ First"))
    (should (string-equal (causal-org-unicode-get :last-column) "⇥ Last"))
    (should (string-equal (causal-org-unicode-get :row) "═"))
    (should (string-equal (causal-org-unicode-get :column) "║"))
    (should (string-equal (causal-org-unicode-get :beginning-of-buffer) "⇱"))
    (should (string-equal (causal-org-unicode-get :end-of-buffer) "⇲"))
    (should (string-equal (causal-org-unicode-get :info-functions) "ⓘ 𝑓(𝑥)"))
    (should (string-equal (causal-org-unicode-get :info) "ⓘ"))
    (should (string-equal (causal-org-unicode-get :clock-in) "🕘 in"))
    (should (string-equal (causal-org-unicode-get :clock-out) "🕔 out"))
    (should (string-equal (causal-org-unicode-get :clock-report) "🕒 🧾"))
    (should (string-equal (causal-org-unicode-get :paragraph) "¶"))
    (should (string-equal (causal-org-unicode-get :update) "⟳"))
    (should (string-equal (causal-org-unicode-get :kill) "×"))))


(provide 'test-causal-org-utils)
;;; test-causal-org-utils.el ends here
