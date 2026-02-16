;;; causal-$MODULE.el --- Transient UI for $MODULE -*- lexical-binding: t; -*-

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

;; This library provides a Transient-based user interface for `$MODULE-mode'.

;; INSTALLATION

;; In your initialization file, bind the Transient `causal-$MODULE-tmenu' to your
;; key binding of preference.

;; (require 'causal-$MODULE) ; optional if using autoloaded menu
;; (keymap-set $MODULE-mode-map "C-o" #'causal-$MODULE-tmenu)

;;; Code:
(require 'causal-$MODULE-settings)
(require 'causal-$MODULE-utils)

;;;###autoload (autoload 'causal-$MODULE-tmenu "causal-$MODULE" nil t)
(transient-define-prefix causal-$MODULE-tmenu ()
  "Causal $MODULE main menu."


  [:class transient-row
   (causal-lib-quit-one)
   ("," "Settings" causal-$MODULE-settings-tmenu)
   ;; ("I" "ⓘ" causal-$MODULE-info)
   ("q" "Quit" quit-window)
   (causal-lib-quit-all)])

(provide 'causal-$MODULE)
;;; causal-$MODULE.el ends here
