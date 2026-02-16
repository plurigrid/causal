;;; causal-ghostty.el --- Transient UI for ghostty-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Plurigrid

;; Author: Plurigrid
;; Keywords: terminal, tools

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

;; Transient-based porcelain for ghostty-emacs (libghostty-vt terminal).
;;
;; This is the 25th Causal porcelain — the first to wrap an external
;; terminal emulator engine rather than a built-in Emacs mode.
;;
;; libghostty-vt provides the VT state machine; this porcelain provides
;; discoverable keyboard-driven menus for terminal management, rendering,
;; and session control.
;;
;; INSTALLATION
;;
;; (require 'causal-ghostty)
;; (keymap-set ghostty-mode-map "C-o" #'causal-ghostty-tmenu)
;;
;; DEPENDENCIES
;;
;; - ghostty-emacs (ghostty.el, ghostty-canvas.el, ghostty-faces.el)
;; - libghostty-vt C module (built from ~/i/libghostty-vt)

;;; Code:

(require 'causal-ghostty-settings)
(require 'causal-ghostty-utils)
(require 'transient)

;; Soft-require ghostty — may not be installed yet
(declare-function ghostty-open "ghostty")
(declare-function ghostty-open-split "ghostty")
(declare-function ghostty-open-vsplit "ghostty")
(declare-function ghostty-send-interrupt "ghostty")
(declare-function ghostty-send-eof "ghostty")
(declare-function ghostty-send-form-feed "ghostty")
(declare-function ghostty-send-return "ghostty")
(declare-function ghostty-send-tab "ghostty")
(declare-function ghostty-send-escape "ghostty")
(declare-function ghostty-scroll-up "ghostty")
(declare-function ghostty-scroll-down "ghostty")
(declare-function ghostty-scroll-to-top "ghostty")
(declare-function ghostty-scroll-to-bottom "ghostty")
(declare-function ghostty--get-terminal-for-buffer "ghostty")
(declare-function ghostty-terminal-write "ghostty")

;;;###autoload (autoload 'causal-ghostty-tmenu "causal-ghostty" nil t)
(transient-define-prefix causal-ghostty-tmenu ()
  "Transient menu for ghostty-emacs terminal."
  :refresh-suffixes t
  ["Causal Ghostty"
   :description (lambda ()
                  (format "Causal Ghostty: %s"
                          (if (and (fboundp 'ghostty--get-terminal-for-buffer)
                                   (ghostty--get-terminal-for-buffer))
                              (format "%dx%d"
                                      (or (bound-and-true-p ghostty-default-cols) 80)
                                      (or (bound-and-true-p ghostty-default-rows) 24))
                            "no terminal")))
   ["Open"
    ("o" "Terminal" ghostty-open)
    ("s" "Split ─" ghostty-open-split)
    ("v" "Split │" ghostty-open-vsplit)]

   ["Send"
    :if causal-ghostty--terminal-active-p
    ("C" "Ctrl+C" ghostty-send-interrupt)
    ("D" "Ctrl+D (EOF)" ghostty-send-eof)
    ("L" "Ctrl+L (Clear)" ghostty-send-form-feed)
    ("e" "Escape" ghostty-send-escape)]

   ["Navigate"
    :if causal-ghostty--terminal-active-p
    ("p" "Scroll Up" ghostty-scroll-up
     :description (lambda () (causal-ghostty-unicode-get :scroll-up))
     :transient t)
    ("n" "Scroll Down" ghostty-scroll-down
     :description (lambda () (causal-ghostty-unicode-get :scroll-down))
     :transient t)
    ("M-<" "Top" ghostty-scroll-to-top)
    ("M->" "Bottom" ghostty-scroll-to-bottom)]

   ["Rendering"
    :if causal-ghostty--terminal-active-p
    ("r" "Refresh" causal-ghostty-force-render)
    ("q" "Quantize" causal-ghostty-cycle-quantization
     :description (lambda ()
                    (format "Quantize: %s"
                            (or (bound-and-true-p causal-ghostty-quantization) "rgb24")))
     :transient t)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("i" "ⓘ›" causal-ghostty-info-tmenu)
   ("," "Settings›" causal-ghostty-settings-tmenu)
   (causal-lib-quit-all)])

(transient-define-prefix causal-ghostty-info-tmenu ()
  "Info menu for Causal Ghostty."
  ["Causal Ghostty ⓘ"
   ["Status"
    ("s" "Module Status" causal-ghostty-show-module-status)
    ("v" "libghostty-vt Version" causal-ghostty-show-vt-version)]

   ["Architecture"
    ("a" "About" causal-ghostty-about)
    ("l" "libghostty-vt" causal-ghostty-info-libghostty)]]

  [:class transient-row
   (causal-lib-quit-one)
   (causal-lib-quit-all)])

(provide 'causal-ghostty)
;;; causal-ghostty.el ends here
