;;; causal-isearch-settings.el --- Causal Re-Builder Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Charles Y. Choi

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
(require 'isearch)
(require 'causal-lib)


(transient-define-prefix causal-isearch-settings-tmenu ()
  "Causal I-Search settings menu."
  :refresh-suffixes t
  ["I-Search: Settings"
    ["Allow"
     ("s" "Scroll" causal-isearch--customize-allow-scroll)
     ("m" "Motion" causal-isearch--customize-allow-motion
      :description (lambda ()
                     (causal-lib-checkbox-label isearch-allow-motion "Motion")))]

    ["Lazy"
     ("h" "Highlight" causal-isearch--customize-lazy-highlight)
     ("c" "Count" causal-isearch--customize-lazy-count
      :description (lambda ()
                     (causal-lib-checkbox-label isearch-lazy-count "Count")))]

    ["Misc"
     ("G" "Group" causal-isearch--customize-group)]]

  [:class transient-row
   (causal-lib-customize-unicode)
   (causal-lib-customize-hide-navigation)]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-isearch-about :transient nil)
   (causal-lib-quit-all)])

(defun causal-isearch--customize-group ()
  "Customize I-Search group."
  (interactive)
  (customize-group "isearch"))

(defun causal-isearch--customize-allow-scroll ()
  "Whether scrolling is allowed during incremental search.

Customizes variable `isearch-allow-scroll'."
  (interactive)
  (customize-variable 'isearch-allow-scroll))

(defun causal-isearch--customize-allow-motion ()
  "Whether to allow movement between isearch matches by cursor motion commands.

Customizes variable `isearch-allow-motion'."
  (interactive)
  (customize-variable 'isearch-allow-motion))

(defun causal-isearch--customize-lazy-count ()
  "Show match numbers in the search prompt.

Customizes variable `isearch-lazy-count'."
  (interactive)
  (customize-variable 'isearch-lazy-count))

(defun causal-isearch--customize-lazy-highlight ()
  "Controls the lazy-highlighting during incremental search.

Customizes variable `isearch-lazy-highlight'."
  (interactive)
  (customize-variable 'isearch-lazy-highlight))

(defun causal-isearch-about-isearch ()
  "Causal I-Search is a Transient menu for I-Search.

Learn more about using Causal I-Search at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal I-Search, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal I-Search was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Causal I-Search.

Always choose love."
  (ignore))

(defun causal-isearch-about ()
  "About information for Causal I-Search."
  (interactive)
  (describe-function #'causal-isearch-about-isearch))

(provide 'causal-isearch-settings)
;;; causal-isearch-settings.el ends here
