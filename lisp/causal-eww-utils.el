;;; causal-eww-utils.el --- Causal EWW Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charles Y. Choi

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
(require 'eww)
(require 'causal-lib)

(defconst causal-eww-unicode-db
  '((:history-back . '("❬" "Back"))
    (:history-forward . '("❭" "Forward"))
    (:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:backward-paragraph . '("¶↑" "Previous ¶"))
    (:forward-paragraph . '("¶↓" "Next ¶"))
    (:forward . '("→" "Forward"))
    (:back . '("←" "Back"))
    (:link . '("🔗" "Link"))
    (:up . '("↑" "Up"))
    (:top . '("⤒" "Top"))
    (:history . '("≣" "History"))
    (:page . '("📄" "Page"))
    (:scroll-up . '("📄↓" "Scroll Up"))
    (:scroll-down . '("📄↑" "Scroll Down"))
    (:follow . '("🚀" "Follow"))
    (:paragraph . '("¶" "Paragraph"))
    (:beginning-of-buffer . '("⇱" "Beginning"))
    (:end-of-buffer . '("⇲" "End"))
    (:reload . '("⟳" "Reload"))
    (:kill . '("×" "Close"))
    (:see-also . '("👀" "See Also")))

  "Unicode symbol DB to use for eww Transient menus.")

(defun causal-eww-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `causal-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (causal-lib-unicode-db-get key causal-eww-unicode-db))

(defun causal-eww-info ()
  "Open Info for Emacs Web Wowser (EWW)."
  (interactive) (info "(eww) Top"))


;; -------------------------------------------------------------------
;;; Commands
(defun causal-eww-forward-paragraph-link ()
  "Move point to first link in next paragraph."
  (interactive)
  (causal-lib-browse-forward-paragraph)
  (shr-next-link))

(defun causal-eww-backward-paragraph-link ()
  "Move point to first link in previous paragraph."
  (interactive)

  (let ((current-line-number (line-number-at-pos)))
    (backward-paragraph)
    (if (= current-line-number (line-number-at-pos))
        (backward-paragraph))
    (shr-next-link)))


;; -------------------------------------------------------------------
;;; Transients

(transient-define-prefix causal-eww-display-tmenu ()
  "Transient menu for EWW display controls.

Note that only the runtime value of variables is changed. Commands in
`causal-eww-settings-tmenu' will persistently change a variable's value."
  ["Causal EWW Display"
   ("f" "Use Fonts" eww-toggle-fonts
    :description (lambda () (causal-lib-checkbox-label shr-use-fonts "Use Fonts"))
    :transient t)
   ("c" "Use Colors" eww-toggle-colors
    :description (lambda () (causal-lib-checkbox-label shr-use-colors "Use Colors"))
    :transient t)
   ("i" "Use Images" eww-toggle-images
    :description (lambda () (causal-lib-checkbox-label shr-inhibit-images "Inhibit Images"))
    :transient t)
   ("d" "Use Direction" eww-toggle-paragraph-direction
    :description (lambda ()
                   (format
                    "¶ direction (%s)"
                    (cond
                     ((eq bidi-paragraph-direction 'left-to-right) "L→R")
                     ((eq bidi-paragraph-direction 'right-to-left) "R←L")
                     ((not bidi-paragraph-direction) "Auto")
                     (t "Undefined"))))
    :transient t)]

  [:class transient-row
   (causal-lib-quit-one)
   (causal-lib-quit-all)])



(provide 'causal-eww-utils)
;;; causal-eww-utils.el ends here
