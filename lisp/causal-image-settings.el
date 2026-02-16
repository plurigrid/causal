;;; causal-image-settings.el --- Causal Image Settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

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

(require 'image-mode)
(require 'cus-edit)
(require 'causal-lib)

(transient-define-prefix causal-image-settings-tmenu ()
  ["Causal Image Settings"
   ["Customize"
    ("G" "Image Group" causal-image--customize-group)]

   ["Causal"
    (causal-lib-customize-unicode)
    (causal-lib-customize-hide-navigation)]]

  [:class transient-row
   (causal-lib-quit-one)
   ;; ("a" "About" causal-info-about :transient nil)
   (causal-lib-quit-all)])

(defun causal-image--customize-group ()
  "Customize Image group."
  (interactive)
  (customize-group "image"))

(provide 'causal-image-settings)
;;; causal-image-settings.el ends here
