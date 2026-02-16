;;; causal-css.el --- Transient UI for CSS mode -*- lexical-binding: t; -*-

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

;; This library provides a Transient-based user interface for `css-mode'.

;; INSTALLATION

;; In your initialization file, bind the Transient `causal-css-tmenu' to your
;; key binding of preference.

;; (require 'causal-css) ; optional if using autoloaded menu
;; (keymap-set css-mode-map "M-m" #'causal-css-tmenu)

;; Note that `causal-css-tmenu' is intended to work with
;; `causal-editkit-main-tmenu' so is given a different binding.

;; It is recommended that Tree-sitter support for CSS is enabled.

;;; Code:
(require 'causal-css-settings)
(require 'causal-css-utils)
(require 'hl-line)

;;;###autoload (autoload 'causal-css-tmenu "causal-css" nil t)
(transient-define-prefix causal-css-tmenu ()
  "Causal menu for `css-mode'.

Transient menu to commands provided by `css-mode'."
  :refresh-suffixes t

  ["Causal CSS"
   ["CSS"
    ("l" "Lookup Symbol" css-lookup-symbol)
    ("c" "Cycle Color" css-cycle-color-format
     :inapt-if (lambda () buffer-read-only)
     :transient t)]

   ["Edit"
    ("f" "Indent CSS Rule" fill-paragraph
     :inapt-if (lambda () buffer-read-only))]

   ["Misc"
    ("h" "Toggle Highlight-line" hl-line-mode
     :description (lambda () (causal-lib-checkbox-label hl-line-mode "Highlight Line"))
     :transient t)]]

  [:class transient-row
   (causal-lib-quit-one)
   ("," "Settings" causal-css-settings-tmenu)
   ("RET" "Done" transient-quit-all)
   (causal-lib-quit-all)])

(provide 'causal-css)
;;; causal-css.el ends here
