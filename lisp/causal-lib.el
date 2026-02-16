;;; causal-lib.el --- Library routines for Causal user interfaces -*- lexical-binding: t; -*-

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

;; Library routines for Causal user interfaces.

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:
(require 'transient)

(defgroup causal nil
  "Settings for Causal user interfaces."
  :group 'convenience)

(defcustom causal-lib-hide-navigation nil
  "If non-nil then hide navigation controls.

If non-nil, customize Causal user interfaces to hide navigation controls for
`transient-quit-all' (control-q) and `transient-quit-one' (control-g)."
  :type 'boolean
  :group 'causal)

(defun causal-lib-customize-causal-lib-hide-navigation ()
  "Customize `causal-lib-hide-navigation'.

Customize Causal user interfaces to hide navigation commands."
  (interactive)
  (customize-variable 'causal-lib-hide-navigation))

(defcustom causal-lib-use-unicode nil
  "If non-nil then use Unicode symbols whenever appropriate for labels."
  :type 'boolean
  :group 'causal)

(defun causal-lib-customize-causal-lib-use-unicode ()
  "Customize `causal-lib-use-unicode'.

Customize Causal user interfaces to use Unicode symbols in place of strings
when appropriate."
  (interactive)
  (customize-variable 'causal-lib-use-unicode))

(defun causal-lib-unicode-db-get (key db)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.
- DB alist containing Unicode symbol map.

 the value of customizable variable `causal-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (let* ((db db)
         (unicode causal-lib-use-unicode)
         (item (alist-get key db)))
    (if unicode
        (nth 0 (eval item))
      (nth 1 (eval item)))))

;; Predicates
(defun causal-lib-display-line-numbers-mode-p ()
  "Predicate to test if `display-line-numbers-mode' is enabled."
  (bound-and-true-p display-line-numbers))

(defun causal-lib-buffer-writeable-p ()
  "Predicate to test if buffer is writeable."
  (not buffer-read-only))

(defun causal-lib-buffer-writeable-and-region-active-p ()
  "Predicate to test if buffer is writeable and region is active."
  (and (causal-lib-buffer-writeable-p) (region-active-p)))

(defun causal-lib-hide-navigation-p ()
  "Predicate for variable `causal-lib-hide-navigation'."
  (if causal-lib-hide-navigation t nil))

(defun causal-lib-quit-all-hide-navigation-p ()
  "Predicate for hiding navigation for the `transient-quit-all' command."
  (if causal-lib-hide-navigation
      t
    (if transient--stack
        nil
      t)))

;; Labels
(defun causal-lib--variable-to-checkbox (v)
  "Checkbox string representation of variable V.
V is either nil or non-nil."
  (if causal-lib-use-unicode
      (if v "☑︎" "◻︎")
    (if v "[x]" "[ ]")))

(defun causal-lib--prefix-label (label prefix)
  "Label constructed with PREFIX and LABEL separated by a space."
  (format "%s %s" prefix label))

(defun causal-lib-checkbox-label (v label)
  "Checkbox label using variable V and LABEL."
  (causal-lib--prefix-label label (causal-lib--variable-to-checkbox v)))

(defun causal-lib--suffix-label (label suffix)
  "Label constructed with LABEL and SUFFIX separated by a space."
  (format "%s %s" label suffix))

(defun causal-lib--quit-one-suffix-label ()
  "Description label for Transient suffix `causal-lib-quit-one'."
  (if transient--stack
      "‹Back"
    "Dismiss"))

(defun causal-lib-browse-backward-paragraph ()
  "Move point backward paragraph such that the first line is highlighted.
\nThis function is intended to be used with `hl-line-mode'."
  (interactive)
  (backward-paragraph 2)
  (forward-line))

(defun causal-lib-browse-forward-paragraph ()
  "Move point forward paragraph such that the first line is highlighted.
\nThis function is intended to be used with `hl-line-mode'."
  (interactive)
  (forward-paragraph)
  (forward-line))

(defun causal-lib-duplicate-file (&optional arg)
  "Duplicate the current file with prefix option ARG.

This command will duplicate and open the current file in the buffer to a
filename of the form “<filename> copy.<extension>”. If the current
buffer is modified, a prompt will be raised to save it before making the
duplicate copy.

By default this command will immediate open the duplicate file into a
new buffer. This can be avoided if a prefix ARG is provided."
  (interactive "P")
  (if (and (buffer-modified-p) (y-or-n-p "Save buffer? "))
      (save-buffer))

  (let ((filename (buffer-file-name)))
    (unless filename
      (error "This command only works on a file"))

    (let* ((extension (file-name-extension filename t))
           (target (format "%s copy%s"
                           (file-name-sans-extension filename)
                           extension)))
      (copy-file filename target)
      (if (not arg)
          (find-file target)))))

;; Transients
(transient-define-suffix causal-lib-quit-all ()
  "Causal suffix to call `transient-quit-all'."
  :transient nil
  :if-not #'causal-lib-quit-all-hide-navigation-p
  :key "C-q"
  :description "Dismiss"
  (interactive)
  (transient-quit-all))

(transient-define-suffix causal-lib-quit-one ()
  "Causal suffix to call `transient-quit-one'."
  :transient nil
  :key "C-g"
  :if-not #'causal-lib-hide-navigation-p
  :description #'causal-lib--quit-one-suffix-label
  (interactive)
  (transient-quit-one))

(transient-define-suffix causal-lib-customize-unicode ()
  "Customize Causal to use Unicode symbols.

This Transient suffix invokes the customize interface for the
variable `causal-lib-use-unicode'.

Note that this variable affects all Causal user interfaces."
  :key "u"
  :transient nil
  :description (lambda ()
                 (causal-lib-checkbox-label
                  causal-lib-use-unicode
                  "Use Unicode Symbols"))
  (interactive)
  (causal-lib-customize-causal-lib-use-unicode))

(transient-define-suffix causal-lib-customize-hide-navigation ()
  "Customize Causal hide navigation controls.

This Transient suffix invokes the customize interface for the
variable `causal-lib-hide-navigation'.

Note that this variable affects all Causal user interfaces."
  :key "n"
  :transient nil
  :description (lambda ()
                 (causal-lib-checkbox-label
                  causal-lib-hide-navigation
                  "Hide Navigation Commands"))
  (interactive)
  (causal-lib-customize-causal-lib-hide-navigation))

(provide 'causal-lib)
;;; causal-lib.el ends here
