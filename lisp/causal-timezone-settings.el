;;; causal-timezone-settings.el --- Causal Timezone Settings -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charles Y. Choi

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
(require 'causal-lib)
(require 'causal-timezone-utils)

(transient-define-prefix causal-timezone-settings-tmenu ()
  "Causal Timezone settings menu."
  ["Timezone: Settings"
   ["Working Hours"
    ("r" "Range" causal-timezone--customize-working-hours-range
     :description (lambda ()
                    (format
                     "Range (%d..%d)"
                     (map-elt causal-timezone-working-hours-range :start)
                     (map-elt causal-timezone-working-hours-range :stop))))

    ("g" "Glyph" causal-timezone--customize-working-hour-glyph
     :description (lambda ()
                    (format
                     "Glyph (%s)"
                     causal-timezone-working-hour-glyph)))

    ("F" "Face" causal-timezone--customize-planner-working-highlight)]

   ["Formats"
    ("c" "Convert" causal-timezone--customize-convert-timestamp-format
     :description (lambda ()
                    (format
                     "Convert: (ex: %s)"
                     (format-time-string
                     causal-timezone-convert-datestamp-format
                     (current-time)))))

    ("p" "Planner" causal-timezone--customize-datestamp-format
     :description (lambda ()
                    (format
                     "Planner: (ex: %s)"
                     (format-time-string
                     causal-timezone-datestamp-format
                     (current-time)))))
    ("f" "Describe Format" causal-timezone--describe-format-time-string)]]

  [:class transient-row
   (causal-lib-customize-unicode)
   (causal-lib-customize-hide-navigation)
   ("D" "Zoneinfo DB" causal-timezone--customize-zone-info-database)]

  [:class transient-row
   (causal-lib-quit-one)
   ("a" "About" causal-timezone-about :transient nil)

   (causal-lib-quit-all)])

(defun causal-timezone--customize-working-hour-glyph ()
  "Set working hour glyph.

This customizes the variable `causal-timezone-working-hour-glyph'."
  (interactive)
  (customize-variable 'causal-timezone-working-hour-glyph))


(defun causal-timezone--customize-zone-info-database ()
  "Set path for zoneinfo database.

This customizes the variable `causal-timezone-zone-info-database'."
  (interactive)
  (customize-variable 'causal-timezone-zone-info-database))

(defun causal-timezone--customize-planner-working-highlight ()
  "Set working hour highlight face.

This customizes the face `causal-timezone-working-highlight'."
  (interactive)
  (customize-face 'causal-timezone-planner-working-highlight))

(defun causal-timezone--customize-working-hours-range ()
  "Set working hours range.

This customizes the variable `causal-timezone-working-hours-range'."
  (interactive)
  (customize-variable 'causal-timezone-working-hours-range))

(defun causal-timezone--customize-convert-timestamp-format ()
  "Set conversion timestamp format.

This customizes the variable `causal-timezone-convert-datestamp-format'."
  (interactive)
  (customize-variable 'causal-timezone-convert-datestamp-format))

(defun causal-timezone--customize-datestamp-format ()
  "Set planner timestamp format.

This customizes the variable `causal-timezone-datestamp-format'."
  (interactive)
  (customize-variable 'causal-timezone-datestamp-format))

(defun causal-timezone--describe-format-time-string ()
  "Describe time string format.

This describes the command `format-time-string'."
  (interactive)
  (describe-command #'format-time-string))

(defun causal-timezone-about ()
  "Causal Timezone is a Transient menu for working with timezones.

Learn more about using Causal Timezone at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/causal/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/causal/issues'

If you enjoy using Causal Timezone, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Causal Timezone was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Causal Timezone.

Always choose love."
  (interactive)
  (describe-function #'causal-timezone-about))

(provide 'causal-timezone-settings)
;;; causal-timezone-settings.el ends here
