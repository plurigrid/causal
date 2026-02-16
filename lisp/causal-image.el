;;; causal-image.el --- Causal Image                 -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Charles Choi

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
(require 'dired)
(require 'causal-image-utils)
(require 'causal-image-settings)

(transient-define-prefix causal-image-tmenu ()
  "Causal Image Main Menu."
  :refresh-suffixes t
  ["Causal Image"
   :description (lambda () (format "Causal Image: %s" (causal-image--identify-label)))
   ["View"
    ("+" "Zoom In" image-increase-size :transient t)
    ("-" "Zoom Out" image-decrease-size :transient t)
    ("o" "Original Size" image-transform-reset-to-original :transient t)
    ("=" "Fit to Window" image-transform-fit-to-window :transient t)
    ("R" "Rotate ⟳90°𝑥…" image-transform-set-rotation
     :description (lambda () (causal-image-unicode-get :rotate))
     :transient t)
    ("%" "% of Original…" image-transform-set-percent :transient t)
    ("." "Reset Point" causal-image--reset-point :transient t)]

   ["Edit"
    :pad-keys t
    :inapt-if (lambda () (if buffer-read-only t nil))
    ("c" "Crop…" image-crop)
    ("f" "Fill…" image-cut)
    ("F" "Set Fill Color…" causal-image--customize-image-cut-color
     :description (lambda () (format "Fill Color (%s)…" image-cut-color)))
    ("r" "Resize›" causal-image-resize-tmenu
     :inapt-if buffer-modified-p)
    ("s" "Save" save-buffer :transient t
     :inapt-if-not buffer-modified-p)
    ("C-s" "Save as…" image-save :transient t)
    ("M-r" "Rename…" rename-visited-file :transient t)
    ("g" "Revert…" revert-buffer :transient t)]

   ["Scroll"
    :pad-keys t
    ("<up>" "Up" image-previous-line
     :description (lambda () (causal-image-unicode-get :scroll-up))
     :transient t)
    ("<down>" "Down" image-next-line
     :description (lambda () (causal-image-unicode-get :scroll-down))
     :transient t)
    ("<left>" "Left" image-backward-hscroll
     :description (lambda () (causal-image-unicode-get :scroll-left))
     :transient t)
    ("<right>" "Right" image-forward-hscroll
     :description (lambda () (causal-image-unicode-get :scroll-right))
     :transient t)]

   ["Edge"
    ("a" "Left ⇤" image-bol
     :description (lambda () (causal-image-unicode-get :edge-left))
     :transient t)
    ("e" "Right ⇥" image-eol
     :description (lambda () (causal-image-unicode-get :edge-right))
     :transient t)
    ("<" "Top-left ⇱" image-bob
     :description (lambda () (causal-image-unicode-get :top-left))
     :transient t)
    (">" "Bottom-right ⇲" image-eob
     :description (lambda () (causal-image-unicode-get :bottom-right))
     :transient t)]]

  [["Traverse"
    ("p" "Previous Image" image-previous-file
     :description (lambda () (causal-image-unicode-get :previous-image))
     :transient t)
    ("n" "Next Image" image-next-file
     :description (lambda () (causal-image-unicode-get :next-image))
     :transient t)
    ("d" "Dired" dired-jump-other-window
     :description (lambda () (causal-image-unicode-get :dired)))]

   ["Mark"
    ("m" "Mark Image" image-mode-mark-file
     :description (lambda () (causal-image-unicode-get :mark-image))
     :transient t)
    ("u" "Unmark Image" image-mode-unmark-file
     :description (lambda () (causal-image-unicode-get :unmark-image))
     :transient t)]

   ["Misc"
    ("w" "Copy filename" image-mode-copy-file-name-as-kill :transient t)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("I" "Identify" causal-image--indentify-verbose)
   ("," "Settings›" causal-image-settings-tmenu)
   ("RET" "Done" transient-quit-all)
   (causal-lib-quit-all)
   ("q" "Quit View" quit-window)])


(provide 'causal-image)
;;; causal-image.el ends here
