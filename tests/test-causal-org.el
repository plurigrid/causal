;;; test-causal-org.el --- Causal Make Tests -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'ert)
(require 'causal-org-test-utils)
(require 'causal-lib-test-utils)
(require 'causal-org)

(ert-deftest test-causal-org-tmenu-heading ()
  (causalt-org-setup)
  (search-forward "* Causal Org Test File" nil t)

  (cl-letf ((causalt-mock #'org-todo)
            (causalt-mock #'org-sort)
            (causalt-mock #'org-clone-subtree-with-time-shift)
            (causalt-mock #'org-insert-heading)
            (causalt-mock #'org-insert-todo-heading)
            (causalt-mock #'org-set-property)
            (causalt-mock #'org-set-tags-command)
            (causalt-mock #'org-schedule)
            (causalt-mock #'org-deadline)
            (causalt-mock #'org-priority-up)
            (causalt-mock #'org-priority-down)
            (causalt-mock #'org-add-note)
            (causalt-mock #'org-refile)
            (causalt-mock #'causal-org-settings-tmenu)
            (causalt-mock #'causal-org-info)
            (causalt-mock #'transient-quit-all))

    (let ((test-vectors
           '((:binding "t" :command org-todo)
             (:binding "s" :command org-sort)
             (:binding "c0" :command org-clone-subtree-with-time-shift)
             (:binding "a" :command org-insert-heading)
             (:binding "T" :command org-insert-todo-heading)
             (:binding "p" :command org-set-property)
             (:binding ":" :command org-set-tags-command)
             (:binding "C-s" :command org-schedule)
             (:binding "C-d" :command org-deadline)
             (:binding "S-<up>" :command org-priority-up)
             (:binding "S-<down>" :command org-priority-down)
             (:binding "n" :command org-add-note)
             (:binding "w" :command org-refile)

             (:binding "," :command causal-org-settings-tmenu)
             (:binding "I" :command causal-org-info)
             (:binding "RET" :command transient-quit-all)
             )))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))


(ert-deftest test-causal-org-tmenu-item ()
  (causalt-org-setup)
  (search-forward "- a" nil t)

  (cl-letf ((causalt-mock #'org-insert-item)
            (causalt-mock #'org-cycle-list-bullet)
            (causalt-mock #'causal-org-toggle-list-to-checkbox)
            (causalt-mock #'causal-org-settings-tmenu)
            (causalt-mock #'causal-org-info)
            (causalt-mock #'transient-quit-all))

    (let ((test-vectors
           '((:binding "a" :command org-insert-item)
             (:binding "b" :command causal-org-toggle-list-to-checkbox)
             (:binding "c" :command org-cycle-list-bullet)
             (:binding "," :command causal-org-settings-tmenu)
             (:binding "I" :command causal-org-info)
             (:binding "RET" :command transient-quit-all))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))

(ert-deftest test-causal-org-tmenu-item-checkbox ()
  (causalt-org-setup)
  (search-forward "- [ ] task 1" nil t)

  (cl-letf ((causalt-mock #'org-ctrl-c-ctrl-c)
            (causalt-mock #'causal-org-checkbox-in-progress)
            (causalt-mock #'causal-org-insert-checkbox)
            (causalt-mock #'org-cycle-list-bullet)
            (causalt-mock #'causal-org-toggle-list-to-checkbox))

    (let ((test-vectors
           '((:binding "a" :command causal-org-insert-checkbox)
             (:binding "b" :command causal-org-toggle-list-to-checkbox)
             (:binding "c" :command org-cycle-list-bullet)
             (:binding "C-c" :command org-ctrl-c-ctrl-c)
             (:binding "-" :command causal-org-checkbox-in-progress))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))

(ert-deftest test-causal-org-tmenu-TBLFM ()
  (causalt-org-setup)
  (search-forward "#+TBLFM" nil t)

  (cl-letf ((causalt-mock #'org-table-recalculate-buffer-tables)
            (causalt-mock #'org-table-edit-formulas))

    (let ((test-vectors
           '((:binding "C-c" :command org-table-recalculate-buffer-tables)
             (:binding "F" :command org-table-edit-formulas))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))

(ert-deftest test-causal-org-tmenu-body ()
  (causalt-org-setup)
  (search-forward "This is" nil t)

  (cl-letf ((causalt-mock #'org-ctrl-c-star)
            (causalt-mock #'org-ctrl-c-minus)
            (causalt-mock #'org-insert-structure-template)
            (causalt-mock #'org-insert-drawer)
            (causalt-mock #'causal-org-insert-keyword)
            )

    (let ((test-vectors
           '((:binding "*" :command org-ctrl-c-star)
             (:binding "-" :command org-ctrl-c-minus)
             (:binding "bc" :command org-insert-structure-template)
             (:binding "d" :command org-insert-drawer)
             (:binding "k" :command causal-org-insert-keyword))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))


(ert-deftest test-causal-org-tmenu-body-keyword ()
  (causalt-org-setup)
  (search-forward "#+PLOT" nil t)

  (cl-letf ((causalt-mock #'org-ctrl-c-ctrl-c))
    (let ((test-vectors
           '((:binding "C-c" :command org-ctrl-c-ctrl-c))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))

(ert-deftest test-causal-org-tmenu-body-in-src-block ()
  (causalt-org-setup)
  (search-forward "(message" nil t)

  (cl-letf ((causalt-mock #'org-ctrl-c-ctrl-c))
    (let ((test-vectors
           '((:binding "C-c" :command org-ctrl-c-ctrl-c))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))


(ert-deftest test-causal-org-tmenu-body-at-property-drawer ()
  (causalt-org-setup)
  (search-forward ":PROPERTIES:" nil t)

  (cl-letf ((causalt-mock #'org-set-property))
    (let ((test-vectors
           '((:binding "p" :command org-set-property))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))


(ert-deftest test-causal-org-tmenu-body-at-property ()
  (causalt-org-setup)
  (search-forward ":CREATED:" nil t)

  (cl-letf ((causalt-mock #'org-set-property)
            (causalt-mock #'org-property-action))
    (let ((test-vectors
           '((:binding "p" :command org-set-property)
             (:binding "a" :command org-property-action))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))


(ert-deftest test-causal-org-tmenu-body-at-drawer ()
  (causalt-org-setup)
  (search-forward ":LOGBOOK:" nil t)

  (cl-letf ((causalt-mock #'org-cycle))
    (let ((test-vectors
           '((:binding "TAB" :command org-cycle))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))

(ert-deftest test-causal-org-tmenu-body-at-clock-log ()
  (causalt-org-setup)
  (search-forward "CLOCK:" nil t)

  (cl-letf ((causalt-mock #'org-clock-in)
            (causalt-mock #'org-clock-timestamps-up)
            (causalt-mock #'org-clock-timestamps-down))
    (let ((test-vectors
           '((:binding "M-c" :command org-clock-in)
             (:binding "u" :command org-clock-timestamps-up)
             (:binding "d" :command org-clock-timestamps-down))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))
(ert-deftest test-causal-org-tmenu-block ()
  (causalt-org-setup)
  (search-forward "#+BEGIN_SRC" nil t)

  (cl-letf ((causalt-mock #'org-ctrl-c-ctrl-c)
            (causalt-mock #'org-edit-src-code)
            (causalt-mock #'causal-org-assign-name))
    (let ((test-vectors
           '((:binding "C-c" :command org-ctrl-c-ctrl-c)
             (:binding "'" :command org-edit-src-code)
             (:binding "n" :command causal-org-assign-name))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))


(ert-deftest test-causal-org-tmenu-utility ()
  (causalt-org-setup)
  (search-forward "* Causal Org Test File" nil t)

  (cl-letf (((symbol-function #'display-graphic-p) (lambda (&optional display) t))
            (causalt-mock #'org-insert-link)
            (causalt-mock #'org-insert-last-stored-link)
            (causalt-mock #'org-cite-insert)
            (causalt-mock #'org-timestamp)
            (causalt-mock #'org-timestamp-inactive)
            (causalt-mock #'org-clock-in)
            (causalt-mock #'org-clock-out)
            (causalt-mock #'org-clock-report)
            (causalt-mock #'org-link-preview)
            (causalt-mock #'causal-org-toggle-images)
            (causalt-mock #'visible-mode)
            (causalt-mock #'prettify-symbols-mode)
            (causalt-mock #'visual-line-mode)
            (causalt-mock #'org-num-mode))
    (let ((test-vectors
           '((:binding "l" :command org-insert-link)
             (:binding "L" :command org-insert-last-stored-link)
             (:binding "r" :command org-cite-insert)
             (:binding "." :command org-timestamp)
             (:binding "i" :command org-timestamp-inactive)
             (:binding "M-c" :command org-clock-in)
             ;; (:binding "M-c" :command org-clock-out) ; TODO test
             ;; (:binding "R" :command org-clock-report) ; TODO test
             (:binding "M-i" :command causal-org-toggle-images)
             (:binding "M-l" :command org-link-preview)
             (:binding "M" :command visible-mode)
             (:binding "P" :command prettify-symbols-mode)
             (:binding "V" :command visual-line-mode)
             (:binding "N" :command org-num-mode))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))


  (causalt-org-breakdown))

(ert-deftest test-causal-org-tmenu-navigation ()
  (causalt-org-setup)
  (search-forward "* Causal Org Test File" nil t)

  (cl-letf ((causalt-mock #'org-shifttab)
            (causalt-mock #'previous-line)
            (causalt-mock #'next-line)
            (causalt-mock #'backward-char)
            (causalt-mock #'forward-char)
            (causalt-mock #'org-beginning-of-line)
            (causalt-mock #'org-end-of-line)
            (causalt-mock #'org-mark-subtree)
            (causalt-mock #'org-mark-element)
            ;; (causalt-mock #'org-copy-visible)
            (causalt-mock #'org-export-dispatch))

    (let ((test-vectors
           '((:binding "TAB" :command org-cycle)
             (:binding "S-TAB" :command org-shifttab)
             (:binding "C-p" :command previous-line)
             (:binding "C-n" :command next-line)
             (:binding "C-b" :command backward-char)
             (:binding "C-f" :command forward-char)
             (:binding "C-a" :command org-beginning-of-line)
             (:binding "C-e" :command org-end-of-line)
             (:binding "ms" :command org-mark-subtree)
             (:binding "me" :command org-mark-element)
             ;; (:binding "v" :command org-copy-visible)
             (:binding "e" :command org-export-dispatch))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))

(ert-deftest test-causal-org-tmenu-navigation-table ()
  (causalt-org-setup)
  (search-forward "|" nil t)
  (push-mark)
  (forward-char 2)

  (cl-letf ((causalt-mock #'org-table-beginning-of-field)
            (causalt-mock #'org-table-end-of-field))

    (let ((test-vectors
           '((:binding "M-a" :command org-table-beginning-of-field)
             (:binding "M-e" :command org-table-end-of-field))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))

(ert-deftest test-causal-org-tmenu-table ()
  (causalt-org-setup)
  (search-forward "|" nil t)
  (push-mark)
  (forward-char 2)

  (cl-letf ((causalt-mock #'causal-org-table-kill-field-as-copy)
            (causalt-mock #'causal-org-table-copy-reference-and-deactivate-dwim)
            (causalt-mock #'rectangle-mark-mode)
            (causalt-mock #'causal-org-table-fill-down)
            (causalt-mock #'causal-org-table-structure-tmenu)
            (causalt-mock #'causal-org-assign-name)
            (causalt-mock #'org-table-export)
            (causalt-mock #'org-plot/gnuplot)
            (causalt-mock #'set-mark-command)
            (causalt-mock #'org-table-edit-field)
            (causalt-mock #'org-yank)
            (causalt-mock #'org-table-eval-formula)
            (causalt-mock #'org-table-blank-field)
            (causalt-mock #'org-table-edit-formulas)

            (causalt-mock #'org-table-copy-region)
            (causalt-mock #'org-table-cut-region)
            (causalt-mock #'org-table-paste-rectangle)
            (causalt-mock #'org-table-recalculate)
            (causalt-mock #'org-table-recalculate-buffer-tables)
            (causalt-mock #'org-table-sum)
            (causalt-mock #'org-table-sort-lines)
            (causalt-mock #'org-table-transpose-table-at-point)
            (causalt-mock #'causal-org-table-info-calc-functions)
            (causalt-mock #'org-table-toggle-column-width)
            (causalt-mock #'org-table-shrink)
            (causalt-mock #'org-table-toggle-coordinate-overlays)
            (causalt-mock #'org-table-header-line-mode)
            (causalt-mock #'visual-line-mode))

    (let ((test-vectors
           '((:binding "w" :command causal-org-table-kill-field-as-copy)
             (:binding "r" :command causal-org-table-copy-reference-and-deactivate-dwim)
             (:binding "m" :command rectangle-mark-mode)
             (:binding "d1" :command causal-org-table-fill-down)
             (:binding "l" :command causal-org-table-structure-tmenu )
             (:binding "n" :command causal-org-assign-name)
             (:binding "E" :command org-table-export)
             (:binding "p" :command org-plot/gnuplot)

             (:binding "C-SPC" :command set-mark-command)

             ;; TODO: handle unmark
             ;; ("SPC" "Unmark" (lambda () (interactive) (deactivate-mark)))
             (:binding "`" :command org-table-edit-field)
             (:binding "C-y" :command org-yank)
             (:binding "=" :command org-table-eval-formula)
             (:binding "DEL" :command org-table-blank-field)
             (:binding "F" :command org-table-edit-formulas)

             (:binding "W" :command org-table-copy-region)
             (:binding "C" :command org-table-cut-region)
             (:binding "Y" :command org-table-paste-rectangle)

             (:binding "c" :command org-table-recalculate)
             (:binding "g" :command org-table-recalculate-buffer-tables)
             (:binding "s" :command org-table-sum)
             (:binding "S" :command org-table-sort-lines)
             (:binding "T" :command org-table-transpose-table-at-point)
             (:binding "f" :command causal-org-table-info-calc-functions)

             (:binding "z" :command org-table-toggle-column-width)
             (:binding "Z" :command org-table-shrink)
             (:binding "t" :command org-table-toggle-coordinate-overlays)
             (:binding "h" :command org-table-header-line-mode)
             (:binding "V" :command visual-line-mode))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))


(ert-deftest test-causal-org-table-structure-tmenu ()
  (causalt-org-setup)
  (search-forward "|" nil t)
  (push-mark)
  (forward-char 2)

  (cl-letf ((causalt-mock #'org-table-beginning-of-field)
            (causalt-mock #'org-table-end-of-field)
            (causalt-mock #'org-table-insert-row)
            (causalt-mock #'org-table-insert-column)
            (causalt-mock #'org-table-insert-hline)
            (causalt-mock #'org-table-kill-row)
            (causalt-mock #'org-table-delete-column)
            (causalt-mock #'org-table-move-row-up)
            (causalt-mock #'org-table-move-row-down)
            (causalt-mock #'org-table-move-column-left)
            (causalt-mock #'org-table-move-column-right)
            (causalt-mock #'causal-org-table-insert-align-left)
            (causalt-mock #'causal-org-table-insert-align-center)
            (causalt-mock #'causal-org-table-insert-align-right)
            (causalt-mock #'causal-org-table-info-width-alignment))

    (let ((test-vectors
           '((:binding "M-a" :command org-table-beginning-of-field)
             (:binding "M-e" :command org-table-end-of-field)
             (:binding "r" :command org-table-insert-row)
             (:binding "c" :command org-table-insert-column)
             (:binding "-" :command org-table-insert-hline)
             (:binding "DEL" :command org-table-kill-row)
             (:binding "M-DEL" :command org-table-delete-column)
             (:binding "M-p" :command org-table-move-row-up)
             (:binding "M-n" :command org-table-move-row-down)
             (:binding "M-b" :command org-table-move-column-left)
             (:binding "M-f" :command org-table-move-column-right)
             (:binding "al" :command causal-org-table-insert-align-left)
             (:binding "ac" :command causal-org-table-insert-align-center)
             (:binding "ar" :command causal-org-table-insert-align-right)
             (:binding "I" :command causal-org-table-info-width-alignment))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-table-structure-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))


(ert-deftest test-causal-org-table-fedit-tmenu ()
  (causalt-org-setup)
  (cl-letf ((causalt-mock #'causal-org-table-fedit-first-row-reference)
            (causalt-mock #'causal-org-table-fedit-last-row-reference)
            (causalt-mock #'causal-org-table-fedit-first-column-reference)
            (causalt-mock #'causal-org-table-fedit-last-column-reference)
            (causalt-mock #'causal-org-table-fedit-first-hline-reference)
            (causalt-mock #'causal-org-table-fedit-second-hline-reference)
            (causalt-mock #'causal-org-table-fedit-hline-range-reference)
            (causalt-mock #'causal-org-table-info-references)
            (causalt-mock #'causal-org-table-info-formula-syntax)
            (causalt-mock #'causal-org-table-info-calc-functions)
            (causalt-mock #'causal-org-table-insert-calc-sum)
            (causalt-mock #'causal-org-table-insert-calc-mean)
            (causalt-mock #'causal-org-table-insert-calc-max)
            (causalt-mock #'causal-org-table-insert-calc-min)
            (causalt-mock #'undo)
            (causalt-mock #'transient-quit-all))

    (let ((test-vectors
           '((:binding "@<" :command causal-org-table-fedit-first-row-reference)
             (:binding "@>" :command causal-org-table-fedit-last-row-reference)
             (:binding "$<" :command causal-org-table-fedit-first-column-reference)
             (:binding "$>" :command causal-org-table-fedit-last-column-reference)
             (:binding "1" :command causal-org-table-fedit-first-hline-reference)
             (:binding "2" :command causal-org-table-fedit-second-hline-reference)
             (:binding "r" :command causal-org-table-fedit-hline-range-reference)
             (:binding "f" :command causal-org-table-info-calc-functions)
             (:binding "s" :command causal-org-table-insert-calc-sum)
             (:binding "m" :command causal-org-table-insert-calc-mean)
             (:binding "a" :command causal-org-table-insert-calc-max)
             (:binding "z" :command causal-org-table-insert-calc-min)
             (:binding "R" :command causal-org-table-info-references)
             (:binding "F" :command causal-org-table-info-formula-syntax)
             (:binding "U" :command undo :transient t)
             (:binding "RET" :command transient-quit-all))))

      (causalt-suffix-testcase-runner test-vectors
                                      #'causal-org-table-fedit-tmenu
                                      '(lambda () (random 5000)))))
  (causalt-org-breakdown))


(provide 'test-causal-org)
;;; test-causal-org.el ends here
