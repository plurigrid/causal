;;; causal-info.el --- Transient UI for Info -*- lexical-binding: t; -*-

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

;; Causal Info is an opinionated Transient-based porcelain for Emacs Info.

;; INSTALLATION
;; (require 'causal-info) ; optional if using autoloaded menu
;; (keymap-set Info-mode-map "C-o" #'causal-info-tmenu)

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:
(require 'transient)
(require 'info)
(require 'causal-lib)

(require 'causal-info-variables)
(require 'causal-info-settings)
(require 'causal-info-utils)

;;; Menus
;;;###autoload (autoload 'causal-info-tmenu "causal-info" nil t)

(transient-define-prefix causal-info-tmenu ()
  "CC Info Transient menu."

  [["Overview"
    ("d" "Directory" Info-directory)
    ("M" "Manual…" info-display-manual)
    ("t" "Top" Info-top-node)
    ("T" "ToC" Info-toc :transient nil)]

   ["Goto"
    ("m" "Menu…" Info-menu)
    ("g" "Node…" Info-goto-node)
    ("i" "Index…" Info-index)
    ("I" "V-index…" Info-virtual-index)]

   ["Search"
    :pad-keys t
    ("C-s" "I-search…" isearch-forward)
    ("s" "Search…" Info-search)
    ("S" "Case sensitive…" Info-search-case-sensitively)
    ("a" "Apropos…" info-apropos)]

   ["History"
    :pad-keys t
    ("L" "History" Info-history :transient nil)
    ("M-[" "⏪️" Info-history-back
     :description (lambda () (causal-info-unicode-get :rewind))
     :transient t)
    ("M-]" "⏩️" Info-history-forward
     :description (lambda () (causal-info-unicode-get :fast-forward))
     :transient t)]

   ["Scroll"
    :pad-keys t
    ("S-SPC" "Scroll Down" Info-scroll-down
           :if display-graphic-p
           :description (lambda ()
                    (causal-info-unicode-get :scroll-down))
           :transient t)

    ("DEL" "Scroll Down" Info-scroll-down
           :if-not display-graphic-p
           :description (lambda ()
                    (causal-info-unicode-get :scroll-down))
           :transient t)

    ("SPC" "Scroll Up" Info-scroll-up
           :description (lambda ()
                    (causal-info-unicode-get :scroll-up))
           :transient t)]]

  ["Navigation"
   ["Link"
    :description (lambda () (causal-info-unicode-get :link))
    ("k" "Previous" Info-prev-reference
     :description (lambda () (causal-info-unicode-get :up-arrow))
     :transient t)
    ("j" "Next" Info-next-reference
     :description (lambda () (causal-info-unicode-get :down-arrow))
     :transient t)]

   ["Paragraph"
    :description (lambda () (causal-info-unicode-get :paragraph))
    ("p" "Previous" causal-lib-browse-backward-paragraph
     :description (lambda () (causal-info-unicode-get :up-arrow))
     :transient t)
    ("n" "Next" causal-lib-browse-forward-paragraph
     :description (lambda () (causal-info-unicode-get :down-arrow))
     :transient t)]

   ["All Nodes"
    ("[" "⏪️⤴️" Info-backward-node
     :description (lambda ()
                    (causal-info-unicode-get :rewind-or-up))
     :transient t)
    ("]" "⏩️⤵️" Info-forward-node
     :description (lambda ()
                    (causal-info-unicode-get :fast-forward-or-down))
     :transient t)]

   ["Peer Nodes"
    ("h" "⏪️" Info-prev
     :description (lambda () (causal-info-unicode-get :rewind))
     :transient t)
    ("l" "⏩️" Info-next
     :description (lambda () (causal-info-unicode-get :fast-forward))
     :transient t)]

   [""
    ("<" "⏮️" Info-top-node
     :description (lambda () (causal-info-unicode-get :first))
     :transient nil)
    (">" "⏭️" Info-final-node
     :description (lambda () (causal-info-unicode-get :last))
     :transient nil)]

   [""
    :pad-keys t
    ("^" "⏫️"  Info-up
     :description (lambda () (causal-info-unicode-get :up))
     :transient t)
    ("RET" "Open" Info-follow-nearest-node :transient t)
    ]]

  ["Quick"
   [("J" "Jump to bookmark…" bookmark-jump :transient nil)
    ("B" "Set bookmark…" bookmark-set :transient nil)
    ("b" "List buffers" ibuffer :transient nil)]

   [("c" "Copy node name" Info-copy-current-node-name :transient nil)
    ("G" "Open node in web…" Info-goto-node-web :transient nil)]

   [:pad-keys t
              ("C-M-n" "New Info in frame" causal-info-new-info-frame
               :transient nil)
              ("M-n" "Clone buffer" clone-buffer :transient nil)]]

  [:class transient-row
          (causal-lib-quit-one)
          ("," "Settings›" causal-info-settings-tmenu)
          ("q" "Quit Info" quit-window)])

(defun causal-info-new-info-frame ()
  "Create new Info manual instance (buffer) in a new frame.

This command creates a new frame populated by the
`info-display-manual' command."
  (interactive)
  (other-frame-prefix)
  (call-interactively #'info-display-manual))

(provide 'causal-info)
;;; causal-info.el ends here
