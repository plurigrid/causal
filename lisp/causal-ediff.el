;;; causal-ediff.el --- Transient UI for Eshell -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Y. Choi

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

;; This library provides a Transient-based user interface for Ediff.

;; INSTALLATION

;; To install Causal Ediff, the function `causal-ediff-install' should be called
;; from your Emacs initialization file. You will also need to bind
;; `causal-ediff-tmenu' to your key binding of preference.

;; (require 'causal-ediff) ; optional if using autoloaded menu
;; (causal-ediff-install) ; run this to enable Causal Ediff
;; (add-hook 'ediff-keymap-setup-hook
;;           (lambda ()
;;             (keymap-set ediff-mode-map "C-o" #'causal-ediff-tmenu)))

;; Notes
;; - `causal-ediff-install' will:
;;   * override advise the function `ediff-janitor'.
;;   * in-memory set the variable `ediff-window-setup-function' to plain.
;; - The way Ediff handles keymaps necessitates the configuration of
;;   `ediff-keymap-setup-hook' as shown above.

;;; Code:
(require 'causal-ediff-settings)
(require 'causal-ediff-utils)

;;;###autoload (autoload 'causal-ediff-tmenu "causal-ediff" nil t)
(transient-define-prefix causal-ediff-tmenu ()
  :refresh-suffixes t
  [["A"
    :pad-keys t
    :description (lambda () (causal-ediff--buffer-description
                        ediff-buffer-A
                        "A"
                        (not ediff-buffer-C)))
    ("ab" "A→B" ediff-copy-A-to-B
     :transient t
     :if (lambda () (and
                (not ediff-buffer-C)
                ediff-buffer-B
                (not (causal-ediff--buffer-read-only-p ediff-buffer-B)))))

    ("ac" "A→C" ediff-copy-A-to-C
     :transient t
     :if (lambda () (and
                ediff-buffer-C
                (not (causal-ediff--buffer-read-only-p ediff-buffer-C)))))

    ("ra" "Restore A"
     (lambda ()
       "Restore and save prior state of buffer A."
       (interactive)
       (causal-ediff--restore-and-save-diff ?a))
     :transient t
     :if (lambda () (and
                (or ediff-buffer-B ediff-buffer-C)
                (not (causal-ediff--buffer-read-only-p ediff-buffer-A)))))

    ("wa" "Save A"
     (lambda ()
       "Save buffer A."
       (interactive)
       (causal-ediff--save-buffer ?a))
     :transient t
     :if (lambda () (and
                (or ediff-buffer-B ediff-buffer-C)
                (not (causal-ediff--buffer-read-only-p ediff-buffer-A))))
     :inapt-if-not (lambda () (buffer-modified-p ediff-buffer-A)))]

   ;; !!!: A diff B
   ["Diff"
    :pad-keys t
    :if (lambda () (not ediff-buffer-C))
    ("p" "↑" ediff-previous-difference
     :description (lambda () (causal-ediff-unicode-get :previous))
     :transient t)
    ("n" "↓" ediff-next-difference
     :description (lambda () (causal-ediff-unicode-get :next))
     :transient t)

    ("<" "↤" ediff-scroll-horizontally
     :description (lambda () (causal-ediff-unicode-get :scroll-to-right))
     :transient t)
    (">" "↦" ediff-scroll-horizontally
     :description (lambda () (causal-ediff-unicode-get :scroll-to-left))
     :transient t)

    ("!" "⟲" ediff-update-diffs
     :description (lambda () (causal-ediff-unicode-get :refresh))
     :transient t)

    ("|" "H/V" ediff-toggle-split
     :transient t
     :description causal-ediff--split-window-vertically-description)
    ("#" "Skip 𝑤𝑠" ediff-toggle-skip-similar
     :transient t
     :description (lambda ()
                    (causal-lib-checkbox-label ediff-ignore-similar-regions
                                               "Skip Space")))]

   ["B"
    :pad-keys t
    :description (lambda () (causal-ediff--buffer-description ediff-buffer-B "B"))
    ("ba" "A←B" ediff-copy-B-to-A
     :transient t
     :if (lambda () (and
                (not (causal-ediff--buffer-read-only-p ediff-buffer-A)))))

    ("bc" "B→C" ediff-copy-B-to-C
     :transient t
     :if (lambda () (and
                ediff-buffer-C
                (not (causal-ediff--buffer-read-only-p ediff-buffer-C)))))

    ("rb" "Restore B"
     (lambda ()
       "Restore and save prior state of buffer B."
       (interactive)
       (causal-ediff--restore-and-save-diff ?b))
     :transient t
     :if (lambda () (and
                ediff-buffer-B
                (not (causal-ediff--buffer-read-only-p ediff-buffer-B))
                (not (string= (file-name-extension (buffer-name ediff-buffer-B))
                              "~{index}")))))

    ("wb" "Save B"
     (lambda ()
       "Save buffer B."
       (interactive)
       (causal-ediff--save-buffer ?b))
     :transient t
     :if (lambda () (and
                (not (causal-ediff--buffer-read-only-p ediff-buffer-B))
                (not (string= (file-name-extension (buffer-name ediff-buffer-B))
                              "~{index}"))))
     :inapt-if-not (lambda () (buffer-modified-p ediff-buffer-B)))]

   ;; !!!: A B diff C
   ["Diff"
    :pad-keys t
    :if (lambda () (and ediff-buffer-C t))
    ("p" "↑" ediff-previous-difference
     :description (lambda () (causal-ediff-unicode-get :previous))
     :transient t)
    ("n" "↓" ediff-next-difference
     :description (lambda () (causal-ediff-unicode-get :next))
     :transient t)

    (">" "↤" ediff-scroll-horizontally
     :description (lambda () (causal-ediff-unicode-get :scroll-to-right))
     :transient t)
    ("<" "↦" ediff-scroll-horizontally
     :description (lambda () (causal-ediff-unicode-get :scroll-to-left))
     :transient t)

    ("!" "⟲" ediff-update-diffs
     :description (lambda () (causal-ediff-unicode-get :refresh))
     :transient t)
    ("|" "H/V" ediff-toggle-split
     :description causal-ediff--split-window-vertically-description
     :transient t)
    ("#" "Skip 𝑤𝑠" ediff-toggle-skip-similar
     :transient t
     :description (lambda ()
                    (causal-lib-checkbox-label ediff-ignore-similar-regions
                                               "Skip Space")))]

   ["C"
    :pad-keys t
    :if (lambda () (if ediff-buffer-C t nil))
    :description (lambda () (causal-ediff--buffer-description ediff-buffer-C "C"))
    ("cb" "B←C" ediff-copy-C-to-B
     :transient t
     :if (lambda () (and
                (not (causal-ediff--buffer-read-only-p ediff-buffer-B))
                (not (string= (file-name-extension (buffer-name ediff-buffer-B))
                              "~{index}")))))

    ("ca" "A←C" ediff-copy-C-to-A
     :transient t
     :if (lambda () (and
                (not (causal-ediff--buffer-read-only-p ediff-buffer-A)))))

    ("mab" "Merge A,B to C" causal-ediff-copy-AB-to-C
     :transient t
     :if (lambda () (string-equal (buffer-name ediff-buffer-C) "*ediff-merge*")))

    ("mba" "Merge B,A to C" causal-ediff-copy-BA-to-C
     :transient t
     :if (lambda () (string-equal (buffer-name ediff-buffer-C) "*ediff-merge*")))

    ("rc" "Restore C"
     (lambda ()
       "Restore and save prior state of buffer C."
       (interactive)
       (causal-ediff--restore-and-save-diff ?c))
     :transient t
     :if (lambda ()
           (if (not (causal-ediff--buffer-read-only-p ediff-buffer-C))
               (if (not (string= (buffer-name ediff-buffer-C) "*ediff-merge*"))
                   t
                 nil)
             nil))
     :inapt-if-not (lambda () (buffer-modified-p ediff-buffer-C)))

    ("rm" "Restore Prior to Last Merge"
     (lambda ()
       "Call `ediff-restore-diff-in-merge-buffer' using current diff.

Note that this command will restore only the state of the *ediff-merge*
buffer prior to the previous merge. To avoid any changes to the
conflicted file, exit Ediff and when prompted to save the merge file,
reply with no."
       (interactive)
       (ediff-restore-diff-in-merge-buffer nil))
     :transient t
     :if (lambda ()
           (if (not (causal-ediff--buffer-read-only-p ediff-buffer-C))
               (if (string= (buffer-name ediff-buffer-C) "*ediff-merge*")
                   t
                 nil)
             nil)))]]

  [:class transient-row
   (causal-lib-quit-one)
   ("i" "Status" ediff-status-info)
   ("I" "ⓘ" ediff-documentation)
   ("," "Settings" causal-ediff-settings-tmenu)
   ("q" "Quit Ediff" ediff-quit)])

;;;###autoload (autoload 'causal-ediff-install "causal-ediff" nil t)
(defun causal-ediff-install ()
  "Install Causal Ediff."
  (interactive)
  (setq causal-ediff--installed-p t)

  ;; CC: I set my Ediff variables in `custom-set-variables'
  ;; Use your own preference.
  ;; '(ediff-keep-variants nil)
  ;; '(ediff-split-window-function 'split-window-horizontally)
  ;; '(ediff-window-setup-function 'ediff-setup-windows-plain)

  (unless (eq ediff-window-setup-function #'ediff-setup-windows-plain)
    (message
     "Overriding ediff-window-setup-function to ediff-setup-windows-plain. \
Consider customizing to always set this variable to plain.")
    (setq ediff-window-setup-function #'ediff-setup-windows-plain))

  (add-hook
   'ediff-before-setup-hook
   #'causal-ediff--stash-window-configuration-for-ediff)
  (add-hook
   'ediff-after-quit-hook-internal
   #'causal-ediff--restore-window-configuration-for-ediff)
  (advice-add 'ediff-janitor :override #'causal-ediff-janitor))

;;;###autoload (autoload 'causal-ediff-uninstall "causal-ediff" nil t)
(defun causal-ediff-uninstall ()
  "Uninstall Causal Ediff."
  (interactive)
  (advice-remove 'ediff-janitor #'causal-ediff-janitor)

  (remove-hook
   'ediff-before-setup-hook
   #'causal-ediff--stash-window-configuration-for-ediff)
  (remove-hook
   'ediff-after-quit-hook-internal
   #'causal-ediff--restore-window-configuration-for-ediff)

  (setq causal-ediff--installed-p nil))

(provide 'causal-ediff)
;;; causal-ediff.el ends here
