;;; causal-help.el --- Transient UI for Help -*- lexical-binding: t; -*-

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

;; This library provides a Transient-based user interface for `help-mode'.

;; INSTALLATION

;; In your initialization file, bind the Transient `causal-help-tmenu' to your
;; key binding of preference.

;; (require 'causal-help) ; optional if using autoloaded menu
;; (keymap-set help-mode-map "C-o" #'causal-help-tmenu)

;; The following keybindings are recommended to support consistent behavior
;; between `help-mode' and `causal-help-tmenu'.

;; (keymap-set help-mode-map "M-[" #'help-go-back)
;; (keymap-set help-mode-map "M-]" #'help-go-forward)
;; (keymap-set help-mode-map "p" #'causal-lib-browse-backward-paragraph)
;; (keymap-set help-mode-map "n" #'causal-lib-browse-forward-paragraph)
;; (keymap-set help-mode-map "P" #'help-goto-previous-page)
;; (keymap-set help-mode-map "N" #'help-goto-next-page)
;; (keymap-set help-mode-map "j" #'forward-button)
;; (keymap-set help-mode-map "k" #'backward-button)

;;; Code:
(require 'bookmark)
(require 'button)
(require 'causal-help-settings)
(require 'causal-help-utils)

;;;###autoload (autoload 'causal-help-tmenu "causal-help" nil t)
(transient-define-prefix causal-help-tmenu ()
  "Causal Help main menu."
  :refresh-suffixes t
  ["Causal Help"
   ["Navigation"
    :pad-keys t
    ("C-p" "Previous" previous-line
     :description (lambda () (causal-help-unicode-get :previous))
     :transient t)
    ("C-n" "Next" next-line
     :description (lambda () (causal-help-unicode-get :next))
     :transient t)
    ("<" "Beginning" beginning-of-buffer
     :description (lambda () (causal-help-unicode-get :beginning-of-buffer))
     :transient t)
    (">" "End" end-of-buffer
     :description (lambda () (causal-help-unicode-get :end-of-buffer))
     :transient t)]

   [""
    ("p" "Backward" causal-lib-browse-backward-paragraph
     :description (lambda () (format "%s %s"
                                     (causal-help-unicode-get :previous)
                                     (causal-help-unicode-get :paragraph)))
     :transient t)
    ("n" "Forward" causal-lib-browse-forward-paragraph
     :description (lambda () (format "%s %s"
                                     (causal-help-unicode-get :next)
                                     (causal-help-unicode-get :paragraph)))
     :transient t)
    ("P" "Backward" help-goto-previous-page
     :description (lambda () (format "%s %s"
                                     (causal-help-unicode-get :previous)
                                     (causal-help-unicode-get :page)))
     :transient t)
    ("N" "Forward" help-goto-next-page
     :description (lambda () (format "%s %s"
                                     (causal-help-unicode-get :next)
                                     (causal-help-unicode-get :page)))
     :transient t)]

   ["History"
    ("M-[" "Previous" help-go-back
     :description (lambda () (causal-help-unicode-get :previous))
     :transient t)
    ("M-]" "Next" help-go-forward
     :description (lambda () (causal-help-unicode-get :next))
     :transient t)]

   ["Link"
    :pad-keys t
    ("j" "Forward" forward-button
     :description (lambda () (causal-help-unicode-get :forward))
     :transient t)
    ("k" "Backward" backward-button
     :description (lambda () (causal-help-unicode-get :backward))
     :transient t)
    ("RET" "Open" push-button)]]

  [["Describe"
    ("ds" "Symbol…" describe-symbol)
    ("dv" "Variable…" describe-variable)]
   [""
    ("dc" "Command…" describe-command)
    ("df" "Function…" describe-function)]

   ["Info"
    :if causal-help--current-data-p
    ("i" "Goto" help-goto-info)
    ("I" "Elisp" help-goto-lispref-info)]

   ["Source"
    ("s" "Source" help-view-source)
    ("c" "Customize" help-customize
     :if causal-help--symbolp)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("," "Settings" causal-help-settings-tmenu)
   ("q" "Quit" quit-window)
   (causal-lib-quit-all)])

(provide 'causal-help)
;;; causal-help.el ends here
