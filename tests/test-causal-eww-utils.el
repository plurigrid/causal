;;; test-causal-eww-utils.el --- Causal Make Utils Tests  -*- lexical-binding: t; -*-

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
(require 'causal-eww-test-utils)
(require 'causal-eww-utils)

(ert-deftest test-causal-eww-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal (causal-eww-unicode-get :history-back) "Back"))
    (should (string-equal (causal-eww-unicode-get :history-forward) "Forward"))
    (should (string-equal (causal-eww-unicode-get :previous) "Previous"))
    (should (string-equal (causal-eww-unicode-get :next) "Next"))
    (should (string-equal (causal-eww-unicode-get :forward-paragraph) "Next ¶"))
    (should (string-equal (causal-eww-unicode-get :backward-paragraph) "Previous ¶"))
    (should (string-equal (causal-eww-unicode-get :forward) "Forward"))
    (should (string-equal (causal-eww-unicode-get :back) "Back"))
    (should (string-equal (causal-eww-unicode-get :link) "Link"))
    (should (string-equal (causal-eww-unicode-get :up) "Up"))
    (should (string-equal (causal-eww-unicode-get :top) "Top"))
    (should (string-equal (causal-eww-unicode-get :history) "History"))
    (should (string-equal (causal-eww-unicode-get :page) "Page"))
    (should (string-equal (causal-eww-unicode-get :scroll-up) "Scroll Up"))
    (should (string-equal (causal-eww-unicode-get :scroll-down) "Scroll Down"))
    (should (string-equal (causal-eww-unicode-get :follow) "Follow"))
    (should (string-equal (causal-eww-unicode-get :paragraph) "Paragraph"))
    (should (string-equal (causal-eww-unicode-get :beginning-of-buffer) "Beginning"))
    (should (string-equal (causal-eww-unicode-get :end-of-buffer) "End"))
    (should (string-equal (causal-eww-unicode-get :reload) "Reload"))
    (should (string-equal (causal-eww-unicode-get :kill) "Close"))
    (should (string-equal (causal-eww-unicode-get :see-also) "See Also")))

  (let ((causal-lib-use-unicode t))
    (should (string-equal (causal-eww-unicode-get :history-back) "❬"))
    (should (string-equal (causal-eww-unicode-get :history-forward) "❭"))
    (should (string-equal (causal-eww-unicode-get :previous) "↑"))
    (should (string-equal (causal-eww-unicode-get :next) "↓"))
    (should (string-equal (causal-eww-unicode-get :forward-paragraph) "¶↓"))
    (should (string-equal (causal-eww-unicode-get :backward-paragraph) "¶↑"))
    (should (string-equal (causal-eww-unicode-get :forward) "→"))
    (should (string-equal (causal-eww-unicode-get :back) "←"))
    (should (string-equal (causal-eww-unicode-get :link) "🔗"))
    (should (string-equal (causal-eww-unicode-get :up) "↑"))
    (should (string-equal (causal-eww-unicode-get :top) "⤒"))
    (should (string-equal (causal-eww-unicode-get :history) "≣"))
    (should (string-equal (causal-eww-unicode-get :page) "📄"))
    (should (string-equal (causal-eww-unicode-get :scroll-up) "📄↓"))
    (should (string-equal (causal-eww-unicode-get :scroll-down) "📄↑"))
    (should (string-equal (causal-eww-unicode-get :follow) "🚀"))
    (should (string-equal (causal-eww-unicode-get :paragraph) "¶"))
    (should (string-equal (causal-eww-unicode-get :beginning-of-buffer) "⇱"))
    (should (string-equal (causal-eww-unicode-get :end-of-buffer) "⇲"))
    (should (string-equal (causal-eww-unicode-get :reload) "⟳"))
    (should (string-equal (causal-eww-unicode-get :kill) "×"))
    (should (string-equal (causal-eww-unicode-get :see-also) "👀"))))

(ert-deftest test-causal-eww-display-tmenu ()
  (let ((tmpfile "causal-eww-display-tmenu.txt"))
    (causalt-eww-setup)
    (cl-letf ((causalt-mock #'eww-toggle-fonts)
              (causalt-mock #'eww-toggle-colors)
              (causalt-mock #'eww-toggle-images)
              (causalt-mock #'eww-toggle-paragraph-direction))

      (let ((test-vectors
             '((:binding "f" :command eww-toggle-fonts)
               (:binding "c" :command eww-toggle-colors)
               (:binding "i" :command eww-toggle-images)
               (:binding "d" :command eww-toggle-paragraph-direction))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-eww-display-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-eww-breakdown)))

(provide 'test-causal-eww-utils)
;;; test-causal-eww-utils.el ends here
