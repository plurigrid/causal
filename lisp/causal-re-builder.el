;;; causal-re-builder.el --- Transient UI for RE-Builder -*- lexical-binding: t; -*-

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

;; Causal RE-Builder is an opinionated Transient-based porcelain for the Emacs
;; regular expression editor.

;; INSTALLATION
;; (require 'causal-re-builder) ; optional if using autoloaded menu
;; (keymap-set reb-mode-map "C-o" #'causal-re-builder-tmenu)
;; (keymap-set reb-lisp-mode-map "C-o" #'causal-re-builder-tmenu)

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:
(require 're-builder)
(require 'rx)
(require 'transient)
(require 'causal-lib)
(require 'causal-re-builder-utils)
(require 'causal-re-builder-settings)

;;;###autoload (autoload 'causal-re-builder-tmenu "causal-re-builder" nil t)
(transient-define-prefix causal-re-builder-tmenu ()
  "Transient menu for RE-Builder commands.

Menu for RE-Builder (`re-builder'), a tool to construct a
regexp interactively.

* References
- Info node `(elisp) Regular Expressions'"
  :refresh-suffixes t
  ["RE-Builder"
   :description (lambda () (format "RE-Builder (%s)" reb-target-buffer))
   ["Copy Regexp"
    ("w" "Interactive" causal-re-builder-copy
     :if causal-re-builder-interactive-export-p
     :transient t)
    ("g" "Interactive grep" causal-re-builder-grep-copy
     :if causal-re-builder-interactive-export-p
     :transient t)
    ("c" "Code" reb-copy :transient t)]

   ["Match"
    ("p" "Previous" reb-prev-match
     :description (lambda () (causal-re-builder-unicode-get :previous))
     :transient t)
    ("n" "Next" reb-next-match
     :description (lambda () (causal-re-builder-unicode-get :next))
     :transient t)]

   ["Change"
    ("x" "Syntax" reb-change-syntax
     :description (lambda () (format "Syntax (%s)" reb-re-syntax))
     :transient t)
    ("b" "Target buffer" reb-change-target-buffer
     :transient t)
    ("t" "Case sensitivity" reb-toggle-case :transient t)]

   ["Display"
    ("s" "Subexp mode" reb-enter-subexp-mode)
    ("f" "Force update" reb-force-update :transient t)]]

  ["Misc"
   ("o" "Occur" causal-reb-occur)]

  [:class transient-row
          (causal-lib-quit-one)
          ("i" "ⓘ Regexp Syntax" causal-re-builder-regexp-info
           :if (lambda () (derived-mode-p 'reb-mode)))
          ("i" "ⓘ Rx Notation" causal-re-builder-rx-info
           :if (lambda () (derived-mode-p 'reb-lisp-mode)))
          ("," "Settings" causal-re-builder-settings-tmenu)
          ("q" "Quit" reb-quit)
          (causal-lib-quit-all)])

(provide 'causal-re-builder)
;;; causal-re-builder.el ends here
