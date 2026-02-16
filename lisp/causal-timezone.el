;;; causal-timezone.el --- Timezone Planner  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

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

(require 'causal-timezone-settings)

;;;###autoload (autoload 'causal-timezone-tmenu "causal-timezone" nil t)
(transient-define-prefix causal-timezone-tmenu ()
  "Main menu for Causal Timezone."

  ["Causal Timezone"
   ("l" "Local to Remote…" causal-timezone-local-time-to-remote)
   ("r" "Remote to Local…" causal-timezone-remote-time-to-local)
   ("z" "Planner…" causal-timezone-planner)]

  [:class transient-row
   (causal-lib-quit-one)
   ("," "Settings" causal-timezone-settings-tmenu)
   (causal-lib-quit-all)])

(provide 'causal-timezone)
;;; causal-timezone.el ends here
