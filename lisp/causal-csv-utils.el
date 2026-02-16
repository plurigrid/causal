;;; causal-csv-utils.el --- Causal CSV Utils -*- lexical-binding: t; -*-

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
(require 'csv-mode)
(require 'causal-lib)
(require 'org-table)

(defconst causal-csv-unicode-db
  '((:up . '("↑" "Up"))
    (:down . '("↓" "Down"))
    (:right . '("→" "Right"))
    (:left . '("←" "Left"))
    (:bol . '("⇤" "Begin"))
    (:eol . '("⇥" "End"))
    (:beginning-of-buffer . '("⇱" "Begin"))
    (:end-of-buffer . '("⇲" "End")))
  "Unicode symbol DB to use for CSV Transient menus.")

(defun causal-csv-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `causal-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (causal-lib-unicode-db-get key causal-csv-unicode-db))

(defun causal-csv-kill-region-as-org-table (start end)
  "Copy CSV region at START, END as Org table in the `kill-ring'."
  (interactive "r")
  (let ((buf (buffer-substring start end)))
    (with-temp-buffer
      (insert buf)
      (org-table-convert-region (point-min) (point-max))
      (kill-region (point-min) (point-max)))))

(defun causal-csv-align-auto ()
  "Auto align CSV fields."
  (interactive)
  (setopt csv-align-style 'auto)
  (call-interactively #'csv-align-fields))

(defun causal-csv-align-left ()
  "Left align CSV fields."
  (interactive)
  (setopt csv-align-style 'left)
  (call-interactively #'csv-align-fields))

(defun causal-csv-align-centre ()
  "Centre align CSV fields."
  (interactive)
  (setopt csv-align-style 'centre)
  (call-interactively #'csv-align-fields))

(defun causal-csv-align-right ()
  "Right align CSV fields."
  (interactive)
  (setopt csv-align-style 'right)
  (call-interactively #'csv-align-fields))


;; -------------------------------------------------------------------
;; Transients
(transient-define-prefix causal-csv-align-tmenu ()
  ["Align"
   :description (lambda () (format
                       "Causal CSV Align: %s %s"
                       (buffer-name)
                       (capitalize (symbol-name csv-align-style))))
   :class transient-row
   ("a" "Auto" causal-csv-align-auto :transient t)
   ("l" "Left" causal-csv-align-left :transient t)
   ("c" "Centre" causal-csv-align-centre :transient t)
   ("r" "Right" causal-csv-align-right :transient t)
   ("t" "Toggle" csv-align-mode :transient t)]

  [:class transient-row
          (causal-lib-quit-one)
          ("RET" "Done" causal-lib-quit-all)
          (causal-lib-quit-all)])

(provide 'causal-csv-utils)
;;; causal-csv-utils.el ends here
