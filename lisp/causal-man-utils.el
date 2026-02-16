;;; causal-man-utils.el --- Causal Man Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Charles Y. Choi

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
(require 'man)
(require 'causal-lib)

(defconst causal-man-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:goto . '("→" "Goto…"))
    (:follow . '("🔗…" "Follow…"))
    (:beginning-of-buffer . '("⇱" "Beginning"))
    (:end-of-buffer . '("⇲" "End"))
    (:paragraph . '("¶" "Paragraph"))
    (:update . '("⟳" "Update"))
    (:kill . '("×" "Close"))
    (:see-also . '("👀" "See Also")))

  "Unicode symbol DB to use for Man Transient menus.")

(defun causal-man-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `causal-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (causal-lib-unicode-db-get key causal-man-unicode-db))

(defun causal-man-occur-options ()
  "Show options for current man page with occur."
  (interactive)
  (occur "^[[:blank:]]*[-+]+[[:alnum:]-=_]*"))

(defun causal-man-info ()
  "Open Info for Emacs Man Page."
  (interactive) (info "(emacs) Man Page"))

(provide 'causal-man-utils)
;;; causal-man-utils.el ends here
