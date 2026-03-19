;;; causal-org-utils.el --- Causal Org Utils -*- lexical-binding: t; -*-

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
(require 'rect)
(require 'org)
(require 'org-num)
(require 'org-element)
(require 'org-element-ast)
(require 'causal-lib)

(defconst causal-org-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:cycle . '("⥅" "Cycle"))
    (:shift-cycle . '("⥆" "S-Cycle"))
    (:up . '("↑" "Up"))
    (:down . '("↓" "Down"))
    (:left . '("←" "Left"))
    (:right . '("→" "Right"))
    (:beginning-of-line . '("⇤" "BoL"))
    (:end-of-line . '("⇥" "EoL"))
    (:beginning-of-line-table . '("⇤" "Begin"))
    (:end-of-line-table . '("⇥" "End"))
    (:beginning-of-field . '("⇤" "Begin"))
    (:end-of-field . '("⇥" "End"))
    (:first-row . '("⤒ First" "First"))
    (:last-row . '("⤓ Last" "Last"))
    (:first-column . '("⇤ First" "First"))
    (:last-column . '("⇥ Last" "Last"))
    (:row . '("═" "Row"))
    (:column . '("║" "Column"))
    (:beginning-of-buffer . '("⇱" "Beginning"))
    (:end-of-buffer . '("⇲" "End"))
    (:info-functions . '("ⓘ 𝑓(𝑥)" "Info f(x)"))
    (:info . '("ⓘ" "Info"))
    (:clock-in . '("🕘 in" "In"))
    (:clock-out . '("🕔 out" "Out"))
    (:clock-report . '("🕒 🧾" "Report"))
    (:paragraph . '("¶" "Paragraph"))
    (:update . '("⟳" "Update"))
    (:kill . '("×" "Close"))
    (:see-also . '("👀" "See Also")))

  "Unicode symbol DB to use for org Transient menus.")

(defun causal-org-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `causal-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (causal-lib-unicode-db-get key causal-org-unicode-db))

;; (defalias 'cc/insert-org-keyword
;;   (kmacro "C-a # + M-x c o m p l e t e - s y m b o l <return>"))

(defun causal-org-insert-keyword ()
  "Insert Org keyword in buffer with completion."
  (interactive)
  (beginning-of-line)
  (insert "#+")
  (call-interactively #'complete-symbol))

(defun causal-org-info ()
  "Open Info for Org manual based on context.

Depending on where the point is, this command will open the point for
Org documentation on the type of section it is in. Sections supported include:

- Headlines
- Plain Lists
- Tables
- Blocks
- Drawers
- Property Syntax
- Timestamps

If none of the above sections are determined at point, then the top
level of the Org manual is opened."
  (interactive)
  (let ((node (cond
               ((org-at-heading-p) "(org) Headlines")
               ((org-at-item-p) "(org) Plain Lists")
               ((or (org-at-table-p) (org-at-TBLFM-p)) "(org) Tables")
               ((org-in-src-block-p) "(org) Working with Source Code")
               ((org-at-block-p) "(org) Blocks")
               ((org-at-drawer-p) "(org) Drawers")
               ((org-at-property-p) "(org) Property Syntax")
               ((org-at-timestamp-p) "(org) Timestamps")
               (t "(org) Top"))))
    (info node)))

(defun causal-org-table-info-references ()
  "Info for Org table references."
  (interactive)
  (info "(org) References" ))

(defun causal-org-table-info-formula-syntax ()
  "Info for Org table formula syntax."
  (interactive)
  (info "(org) Formula syntax for Calc"))

(defun causal-org-table-info-calc-functions ()
  "Info for Calc functions."
  (interactive)
  (info "(calc) Function Index"))

(defun causal-org-table-info-width-alignment ()
  "Info for Org table width and alignment."
  (interactive)
  (info "(org) Column Width and Alignment"))

;; TODO: not clear why this is needed to get the Transient invocation to work.
(defun causal-org-deactivate-mark ()
  "Deactivate mark using function `deactivate-mark'."
  (interactive)
  (deactivate-mark))

(defun causal-org-mode-p ()
  "Predicate to check if the current mode is `org-mode'."
  (derived-mode-p 'org-mode))


;; -------------------------------------------------------------------
;; Org List Functions

(defun causal-org-checkbox-in-progress ()
  "If point is on an Org list item, set it to be a checkbox in-progress."
  (interactive)
  (if (org-at-item-checkbox-p)
      (org-ctrl-c-ctrl-c '(16))
    (org-ctrl-c-ctrl-c '(4))))

(defun causal-org-toggle-list-to-checkbox ()
  "If point is on an Org list item, toggle if the list item is also a checkbox.
Note that this function does not toggle the actual value of a checkbox,
which is done with `org-ctrl-c-ctrl-c'."
  (interactive)
  (org-ctrl-c-ctrl-c '(4)))

(defun causal-org-insert-checkbox ()
  "Insert Org checkbox using `org-insert-item'."
  (interactive)
  (org-insert-item t))


;; -------------------------------------------------------------------
;; Org Block & Table Functions

(defun causal-org-assign-name (name)
  "Insert Org NAME keyword for block or table."
  (interactive "sName: ")
  (unless (or (org-at-table-p) (org-at-block-p))
    (error "Not in a block or table"))

  (org-backward-paragraph)
  (org-end-of-line)
  (insert (format "\n#+NAME: %s" name)))

(defun causal-org-table--insert-calc-formula (fn)
  "Base insert formula for inserting a Calc function FN."
  (let ((formula (format "%s()" fn)))
    (insert formula)
    (backward-char 1)))

;; TODO: Maybe support region input
(defun causal-org-table-insert-calc-sum ()
  "Insert into buffer Calc vector sum ‘vsum()’ function."
  (interactive)
  (let ((fn "vsum"))
    (causal-org-table--insert-calc-formula fn)))

(defun causal-org-table-insert-calc-mean ()
  "Insert into buffer Calc vector mean ‘vmean()’ function."
  (interactive)
  (let ((fn "vmean"))
    (causal-org-table--insert-calc-formula fn)))

(defun causal-org-table-insert-calc-max ()
  "Insert into buffer Calc vector max ‘vmax()’ function."
  (interactive)
  (let ((fn "vmax"))
    (causal-org-table--insert-calc-formula fn)))

(defun causal-org-table-insert-calc-min ()
  "Insert into buffer Calc vector min ‘vmin()’ function."
  (interactive)
  (let ((fn "vmin"))
    (causal-org-table--insert-calc-formula fn)))

(defun causal-org-table--insert-column-alignment (align)
  "Base insert Org table alignment specifier ALIGN."
  (let ((formula (format "<%s>" align)))
    (insert formula)))

(defun causal-org-table-insert-align-left ()
  "Insert into buffer an Org table left alignment specifier ‘<l>’.

This command should only be invoked in an empty table cell."
  (interactive)
  (let ((fn "l"))
    (causal-org-table--insert-column-alignment fn)))

(defun causal-org-table-insert-align-center ()
  "Insert into buffer an Org table center alignment specifier ‘<c>’.

This command should only be invoked in an empty table cell."
  (interactive)
  (let ((fn "c"))
    (causal-org-table--insert-column-alignment fn)))

(defun causal-org-table-insert-align-right ()
  "Insert into buffer an Org table right alignment specifier ‘<r>’.

This command should only be invoked in an empty table cell."
  (interactive)
  (let ((fn "r"))
    (causal-org-table--insert-column-alignment fn)))


;; -------------------------------------------------------------------
;; Org Section Descriptions

(defun causal-org--block-description ()
  "Description string for an Org block."
  (unless (causal-org-mode-p)
    (throw 'causal-org--description-exception "Org Block"))

  (let* ((context (org-element-context))
         (context-type (org-element-type context)))
    (cond
     ((eq context-type 'src-block)
      (format "Org Source (%s)"
              (org-element-property
               :language
               context)))

     ((eq context-type 'example-block)
      "Org Example Block")

     ((eq context-type 'export-block)
      (format "Org Export (%s)"
              (org-element-property
               :type
               context)))

     ((eq context-type 'center-block)
      "Org Center Block")

     ((eq context-type 'quote-block)
      "Org Quote Block")

     ((eq context-type 'verse-block)
      "Org Verse Block")

     ((eq context-type 'dynamic-block)
      "Org Dynamic Block")

     (t "Org Block: Unknown"))))

(defun causal-org--body-description ()
  "Description string for Org body."
  (unless (causal-org-mode-p)
    (throw 'causal-org--description-exception "-Org Body-"))

  (cond
   ((org-at-property-drawer-p)
    "Org Property Drawer")

   ((org-at-drawer-p)
    (let ((key (org-element-property :drawer-name (org-element-context)))
          (structure-type "Drawer"))
      (if key
          (format "Org %s: %s" structure-type key)
        (format "Org %s" structure-type))))

   ((org-at-property-p)
    (let ((key (org-element-property :key (org-element-context)))
          (structure-type "Property"))
      (if key
          (format "Org %s: %s" structure-type key)
        (format "Org %s" structure-type))))

   ((org-at-clock-log-p)
    "Org Clock Log")

   ((org-in-src-block-p)
    "Org Source Body")

   (t
    (let ((heading (org-get-heading t nil t t)))
      (if heading
          (format "Org Body: %s" (substring-no-properties heading))
        (format "Org Body: %s" (buffer-name)))))))

(defun causal-org--keyword-description ()
  "Description string for Org keyword."
  ;; TODO: deal with affiliate keywords like PLOT.
  (unless (causal-org-mode-p)
    (throw 'causal-org--description-exception "-Org Keyword-"))

  (cond
   ((org-at-TBLFM-p) "Org Table Formula (TBLFM)")
   (t (let* ((context (org-element-context))
             (key (org-element-property :key context)))
        (if key
            (format "Org Keyword: %s" key)
          "Org Keyword")))))

(defun causal-org--heading-description ()
  "Description string for Org heading."
  (unless (causal-org-mode-p)
    (throw 'causal-org--description-exception "-Org Headline-"))

  (let ((heading (org-get-heading t nil t t)))
    (if heading
        (format "Org Headline: %s" (substring-no-properties heading))
      (format "Org: %s" (buffer-name)))))

(defun causal-org--item-description ()
  "Description string for Org item."
  (unless (causal-org-mode-p)
    (throw 'causal-org--description-exception "-Org Item-"))

  (let* ((context (org-element-context))
         (start (org-element-contents-begin context))
         (end (org-element-contents-end context))
         (buf (if (and start end)
                  (buffer-substring-no-properties start end)
                nil)))
    (if buf
        (format "Org Item: %s"(nth 0 (string-split buf "\n")))
      "Org Item")))


;; -------------------------------------------------------------------
;; Org Table Functions

(defun causal-org-table--cell-at-point ()
  "At point, return the cell object from an Org table.

A cell object is defined to be a list containing the row and the
column, successively."
  (if (not (org-at-table-p))
      (error "Not in a table"))

  (let* ((row (org-table-current-dline))
         (col (org-table-current-column)))
    (list row col)))

(defun causal-org-table--format-field-reference (cell)
  "Format CELL object into @r$c format.

CELL object obtained via `causal-org-table--cell-at-point'.

See Info node `(org) References' for more on Org table field
reference format."
  (let ((row (nth 0 cell))
        (col (nth 1 cell)))
    (format "@%d$%d" row col)))

(defun causal-org-table--range ()
  "Return range object from a region defined within an Org table.

A range object is a list of two cells computed via
`causal-org-table--cell-at-point', the first being the cell at the
start of the region and the last being the cell at the end of the
region."
  (if (not (and (org-at-table-p) (use-region-p)))
      (error "Not in an Org table"))

  (let* ((end-cell (causal-org-table--cell-at-point))
         (start (region-beginning))
         (end (region-end)))
      (exchange-point-and-mark)
      (let ((start-cell (causal-org-table--cell-at-point)))
        (push-mark start nil t)
        (goto-char end)
        (list start-cell end-cell))))

(defvar causal-org-table--last-reference nil
  "Last stored Org table reference.

State variable to store an Org table reference (field or range)
to be used in an Org table formula. This variable is set via
`causal-org-table--reference-dwim'

NOTE: This state variable to work-around my lack of clarity on
region and mouse menu interaction.")

(defun causal-org-table--reference-dwim ()
  "Org table reference given point or region is defined.

Return Org table reference (field or range) depending on whether
a point or region is defined in an Org table.

If the region is defined over multiple columns, then a Calc
vector matrix is returned. See Info node `(org) Formula syntax
for Calc' for more.

Calling this function will set `causal-org-table--last-reference'.

See Info node `(org) References' for more on Org table field
reference format."
  (if (not (org-at-table-p))
      (error "Not in an Org table"))

  (cond
   ((use-region-p)

    (let* ((range (causal-org-table--range))
           (start (nth 0 range))
           (end (nth 1 range))
           (msg (format "%s..%s"
                        (causal-org-table--format-field-reference start)
                        (causal-org-table--format-field-reference end))))
      (setq causal-org-table--last-reference (causal-org-table--range-to-reference range))
      msg))

   (t
    (let ((msg (causal-org-table--format-field-reference (causal-org-table--cell-at-point))))
      (setq causal-org-table--last-reference msg)
      msg))))

(defun causal-org-table-copy-reference-dwim ()
  "Copy Org table reference (field or range) into kill ring.

Given a point or region defined in an Org table, add to the
`kill-ring' an Org table field or range reference.

If the region is defined over multiple columns, then a Calc vector
matrix is returned. See Info node `(org) Formula syntax for Calc' and
Info node `(calc) Vectors and Matrices' for more.

If the buffer *Edit Formulas* is available (usually via
`org-table-edit-formulas'), the reference will be inserted into
it.

See Info node `(org) References' for more on Org table field
reference format."
  (interactive)
  (if (not (org-at-table-p))
      (error "Not in an Org table"))

  (let ((msg (causal-org-table--reference-dwim))
        (formulas-buffer (get-buffer "*Edit Formulas*")))
    (if formulas-buffer
        (with-current-buffer formulas-buffer
          (insert causal-org-table--last-reference)))
    (message "Range: %s, Copied %s" msg causal-org-table--last-reference)
    (kill-new causal-org-table--last-reference)))

(defun causal-org-table-copy-reference-and-deactivate-dwim ()
  "Copy Org table reference (field or range) into kill ring and deactivate mark.

Given a point or region defined in an Org table, add to the `kill-ring'
an Org table field or range reference.

If the region is defined over multiple columns, then a Calc vector
matrix is returned. See Info node `(org) Formula syntax for Calc' and
Info node `(calc) Vectors and Matrices' for more.

If the buffer *Edit Formulas* is available (usually via
`org-table-edit-formulas'), the reference will be inserted into it.

See Info node `(org) References' for more on Org table field
reference format."
  (interactive)
  (causal-org-table-copy-reference-dwim)
  (if mark-active
      (deactivate-mark)))

(defun causal-org-table--range-to-reference (range)
  "Convert RANGE object to Org table reference (field or range).

If the region is defined over multiple columns, then a Calc
vector matrix is returned. See Info node `(org) Formula syntax
for Calc' for more.

See `causal-org-table--range' for more on RANGE object."
  (let* ((start (nth 0 range))
         (end (nth 1 range))
         (a (nth 0 start))
         (b (nth 1 start))
         (c (nth 0 end))
         (d (nth 1 end))

         (r1 (apply #'min (list a c)))
         (c1 (apply #'min (list b d)))

         (r2 (apply #'max (list a c)))
         (c2 (apply #'max (list b d)))

         (rowrange (number-sequence r1 r2))
         (buflist (list)))


    (cond
     ((and (= r1 r2) (= c1 c2))
      (format "@%d$%d" r1 c1 ))

     ((or (= c1 c2) (= r1 r2))
      (format "@%d$%d..@%d$%d" r1 c1 r2 c2))

     (t
      (mapc (lambda (r)
              (push (format "@%d$%d..@%d$%d" r c1 r c2) buflist))
            rowrange)

      (format "vec(%s)"
              (string-join (reverse buflist) ", "))))))


(defun causal-org-table-fill-down (rows)
  "Fill table down with count of ROWS."
  (interactive "nRows: ")
  (while (> rows 0)
      ;;(message "%d" rows)
      (call-interactively #'org-table-copy-down)
      (setq rows (1- rows))))

(defun causal-org-table-kill-field-as-copy ()
  "Kill field as copy."
  (interactive)
  (let ((value (string-trim (org-table-get-field))))
    (message "Copied '%s' to kill ring." value)
    (kill-new value)))


;; -------------------------------------------------------------------
;; fedit functions

(defun causal-org-table-fedit-first-row-reference ()
  "First row reference."
  (interactive)
  (insert "@<"))

(defun causal-org-table-fedit-last-row-reference ()
  "Last row reference."
  (interactive)
  (insert "@<"))

(defun causal-org-table-fedit-first-column-reference ()
  "First column reference."
  (interactive)
  (insert "$<"))

(defun causal-org-table-fedit-last-column-reference ()
  "Last column reference."
  (interactive)
  (insert "$>"))

(defun causal-org-table-fedit-first-hline-reference ()
  "First hline reference."
  (interactive)
  (insert "@I"))

(defun causal-org-table-fedit-second-hline-reference ()
  "Second hline reference."
  (interactive)
  (insert "@II"))

(defun causal-org-table-fedit-hline-range-reference ()
  "Horizontal range reference."
  (interactive)
  (insert "@I..@II"))


;; -------------------------------------------------------------------
;; Image Preview

(defun causal-org-toggle-images ()
  "Toggle display of all link images in an Org buffer.

This command mimicks ‘org-toggle-inline-images’ which was obsoleted in
Org 9.8."
  (interactive)
  (if (org-link-preview--get-overlays)
      (org-link-preview '(64))
    (org-link-preview 11)))


;; -------------------------------------------------------------------
;; Transients

;; Transient Groups
(transient-define-group causal-org-table-group
  ["Org Table"
   :if (lambda () (and (causal-org-mode-p) (org-at-table-p)))
   :description (lambda () (format "Org Table: %s" (causal-org-table--reference-dwim)))
   ["Table"
    :inapt-if causal-lib-buffer-read-only-p
    ("w" "Copy Field" causal-org-table-kill-field-as-copy
     :inapt-if use-region-p
     :transient t)
    ("r" "Copy Reference" causal-org-table-copy-reference-and-deactivate-dwim
     :transient t)
    ("m" "Mark Rectangle" rectangle-mark-mode :transient t)
    ;; ("{" "Toggle Debugger" org-table-toggle-formula-debugger :transient t)
    ("d" "Fill Down…" causal-org-table-fill-down :transient nil)
    ("l" "Layout›" causal-org-table-structure-tmenu )
    ("n" "Assign Name…" causal-org-assign-name)
    ("E" "Export…" org-table-export)
    ("p" "Plot" org-plot/gnuplot :transient t)]

   ["Edit"
    :pad-keys t
    ("C-SPC" "Mark" set-mark-command :transient t)
    ("SPC" "Unmark" causal-org-deactivate-mark
     :inapt-if (lambda () (if mark-active nil t))
     :transient t)
    ("`" "Field" org-table-edit-field
     :inapt-if causal-lib-buffer-read-only-p
     :transient nil)
    ("C-y" "Paste (yank)" org-yank
     :inapt-if causal-lib-buffer-read-only-p
     :transient t)
    ("=" "Formula*" org-table-eval-formula
     :inapt-if causal-lib-buffer-read-only-p
     :transient t
     :description (lambda () (if prefix-arg "Field Formula" "Column Formula✦")))
    ("DEL" "Blank" org-table-blank-field
     :inapt-if causal-lib-buffer-read-only-p
     :transient t)
    ("F" "Formulas" org-table-edit-formulas
     :inapt-if causal-lib-buffer-read-only-p
     :transient nil)]

   ;; !!! these commands alter the region selected which breaks reading a table
   ;; !!! cell. As such they can not be supported until these commands are
   ;; !!! fixed.

   ["Region"
    ("W" "Copy" org-table-copy-region :transient t)
    ("C" "Cut" org-table-cut-region
     :inapt-if causal-lib-buffer-read-only-p
     :transient t)
    ("Y" "Paste" org-table-paste-rectangle
     :inapt-if causal-lib-buffer-read-only-p
     :transient t)]

   ["Compute"
    :inapt-if causal-lib-buffer-read-only-p
    ("c" "Row" org-table-recalculate
     :description (lambda () (if prefix-arg "Table" "Row✦"))
     :transient t)
    ("g" "All" org-table-recalculate-buffer-tables :transient t)
    ("s" "Sum" org-table-sum :transient t)
    ("S" "Sort" org-table-sort-lines :transient t)
    ("T" "Transpose" org-table-transpose-table-at-point :transient t)
    ("f" "ⓘ 𝑓(𝑥)" causal-org-table-info-calc-functions
     :description (lambda () (format "%s 𝑓(𝑥)" (causal-org-unicode-get :info))))]

   ["Display"
    ("z" "Shrink Column" org-table-toggle-column-width :transient t)
    ("Z" "Shrink Table" org-table-shrink :transient t)
    ("t" "Toggle Coordinates" org-table-toggle-coordinate-overlays
     :description (lambda () (causal-lib-checkbox-label
                         org-table-overlay-coordinates
                         "@𝑟$𝑐"))
     :transient t)
    ("h" "Header Mode" org-table-header-line-mode
     :description (lambda () (causal-lib-checkbox-label
                         org-table-header-line-mode
                         "Header"))
     :transient t)
    ("V" "Line Wrap" visual-line-mode
     :description (lambda () (causal-lib-checkbox-label
                         visual-line-mode
                         "Line Wrap"))
     :transient t)]])


;; TODO: ("c" "Capture…" org-capture)
(transient-define-group causal-org-heading-group
  ["Org Heading"
   :if (lambda () (and (causal-org-mode-p) (org-at-heading-p)))
   :inapt-if causal-lib-buffer-read-only-p
   :description (lambda () (catch 'causal-org--description-exception
                        (causal-org--heading-description)))
   ["Headline"
    :pad-keys t
    ("t" "TODO State…" org-todo)
    ("s" "Sort…" org-sort)
    ("c" "Clone…" org-clone-subtree-with-time-shift)]

   ["Add"
    ("a" "Headline" org-insert-heading)
    ("T" "TODO" org-insert-todo-heading)]

   ["Annotate"
    ("p" "Property…" org-set-property)
    (":" "Tags…" org-set-tags-command)]

   ["Date"
    ("C-s" "Schedule…" org-schedule)
    ("C-d" "Deadline…" org-deadline)]

   ["Priority"
    :pad-keys t
    ("S-<up>" "↑" org-priority-up
     :description (lambda () (causal-org-unicode-get :up))
     :transient t)
    ("S-<down>" "↓" org-priority-down
     :description (lambda () (causal-org-unicode-get :down))
     :transient t)]

   ["Misc"
    ("n" "Note…" org-add-note)
    ("w" "Refile…" org-refile)]])


(transient-define-group causal-org-item-group
  ["Org Item"
   :if (lambda () (and (causal-org-mode-p) (org-at-item-p)))
   :description (lambda () (catch 'causal-org--description-exception
                        (causal-org--item-description)))

   ["Item"
    :description (lambda () (if (org-at-item-checkbox-p)
                           "Checkbox"
                         "Item"))
    :pad-keys t
    :inapt-if causal-lib-buffer-read-only-p
    ("a" "Add" org-insert-item
     :if-not org-at-item-checkbox-p
     :transient t)
    ("a" "Add" causal-org-insert-checkbox
     :if org-at-item-checkbox-p
     :transient t)
    ("b" "Toggle Checkbox" causal-org-toggle-list-to-checkbox
     :description (lambda () (if (org-at-item-checkbox-p)
                            "To Item"
                          "To Checkbox"))
     :transient t)
    ("c" "Cycle" org-cycle-list-bullet :transient t)]

   [""
    :pad-keys t
    :if org-at-item-checkbox-p
    :inapt-if causal-lib-buffer-read-only-p
    ("C-c" "Toggle" org-ctrl-c-ctrl-c
     :transient nil)
    ("-" "In Progress" causal-org-checkbox-in-progress :transient nil)]

   [""
    :inapt-if causal-lib-buffer-read-only-p
    ("s" "Sort…" org-sort)]])



(transient-define-group causal-org-block-group
  ["Org Block"
   :if (lambda () (and (causal-org-mode-p) (org-at-block-p)))
   :inapt-if causal-lib-buffer-read-only-p
   :description (lambda () (catch 'causal-org--description-exception
                        (causal-org--block-description)))
   [("'" "Edit" org-edit-src-code :transient nil)]
   [("n" "Assign Name…" causal-org-assign-name)]
   [("C-c" "Eval" org-ctrl-c-ctrl-c
     :if (lambda () (or (eq (org-element-type (org-element-context)) 'src-block)
                   (eq (org-element-type (org-element-context)) 'dynamic-block)))
     :transient t)]])


(transient-define-group causal-org-body-group
  ["Org Body"
   :if-not (lambda () (if (causal-org-mode-p)
                      (or (org-at-heading-or-item-p)
                          (org-at-table-p)
                          (org-at-block-p)
                          (org-at-keyword-p))
                   t))
   :description (lambda () (catch 'causal-org--description-exception
                        (causal-org--body-description)))
   :inapt-if causal-lib-buffer-read-only-p

   ;; !!!: Body
   ["To"
    :if-not (lambda () (or (org-at-keyword-p)
                      (org-at-drawer-p) ; covers property-drawer-p
                      (org-at-clock-log-p)
                      (org-in-src-block-p)
                      (org-at-property-p)))
    ("*" "Heading" org-ctrl-c-star :transient t)
    ("-" "Item" org-ctrl-c-minus :transient t)]

   ["Add"
    :if-not (lambda () (or (org-at-keyword-p)
                      (org-at-drawer-p) ; covers property-drawer-p
                      (org-at-clock-log-p)
                      (org-in-src-block-p)
                      (org-at-property-p)))
    ("b" "Block…" org-insert-structure-template)
    ("d" "Drawer…" org-insert-drawer)
    ("k" "Keyword…" causal-org-insert-keyword)]

   ;; !!!: org-in-src-block-p
   [:if (lambda () (and (causal-org-mode-p) (org-in-src-block-p)))
    ("'" "Edit" org-edit-src-code :transient nil)]

   [:if (lambda () (and (causal-org-mode-p) (org-in-src-block-p)))
    ("C-c" "Eval" org-ctrl-c-ctrl-c
     :if (lambda () (or (eq (org-element-type (org-element-context)) 'src-block)
                   (eq (org-element-type (org-element-context)) 'dynamic-block)))
     :transient t)]

   ;; !!!: org-at-property-drawer-p
   [:if (lambda () (and (causal-org-mode-p) (org-at-property-drawer-p)))
    ("p" "Add Property…" org-set-property)]

   ;; !!!: org-at-property-p
   [:if (lambda () (and (causal-org-mode-p) (org-at-property-p)))
    ("p" "Add Property…" org-set-property)]

   [:if (lambda () (and (causal-org-mode-p) (org-at-property-p)))
    ("a" "Action…" org-property-action)]

   ;; !!!: org-at-drawer-p
   [:if (lambda () (and (causal-org-mode-p)
                   (org-at-drawer-p)
                   (not (org-at-property-drawer-p))))
    ("TAB" "Cycle…" org-cycle :transient t)]

   ;; !!!: org-at-clock-log-p
   ["Clock"
    :pad-keys t
    :if (lambda () (and (causal-org-mode-p) (org-at-clock-log-p)))
    ("M-c" "🕘 in" org-clock-in
     :description (lambda () (causal-org-unicode-get :clock-in))
     :if-not org-clocking-p)
    ("M-c" "🕔 out" org-clock-out
     :description (lambda () (causal-org-unicode-get :clock-out))
     :if org-clocking-p)]

   ["Timestamp"
    :if (lambda () (and (causal-org-mode-p) (org-at-clock-log-p)))
    ("u" "Adjust Up" org-clock-timestamps-up
     :description (lambda () (format "Adjust %s" (causal-org-unicode-get :up)))
     :transient t)
    ("d" "Adjust Down" org-clock-timestamps-down
     :description (lambda () (format "Adjust %s" (causal-org-unicode-get :down)))
     :transient t)]])


(transient-define-group causal-org-keyword-group
  ["Org Keyword"
   :if (lambda () (and (causal-org-mode-p) (org-at-keyword-p)))
   :description (lambda () (catch 'causal-org--description-exception
                        (causal-org--keyword-description)))
   :inapt-if causal-lib-buffer-read-only-p
   [:if org-at-TBLFM-p
    ("F" "Edit Formulas" org-table-edit-formulas :transient nil)]

   [:if org-at-TBLFM-p
    ("C-c" "Eval" org-table-recalculate-buffer-tables :transient nil)]

   ;; TODO: Does this apply to all affiliate keywords?
   [:if-not org-at-TBLFM-p
    :inapt-if (lambda () (org-element-property :key (org-element-context)))
    ("C-c" "Eval" org-ctrl-c-ctrl-c :transient nil)]])


(transient-define-group causal-org-navigation-group
  [:if causal-org-mode-p
   ["Field"
    :if org-at-table-p
    ("M-a" "⇤" org-table-beginning-of-field
     :description (lambda () (causal-org-unicode-get :beginning-of-field))
     :transient t)]

   [""
    :if org-at-table-p
    ("M-e" "⇥" org-table-end-of-field
     :description (lambda () (causal-org-unicode-get :end-of-field))
     :transient t)]

   ["Mark"
    :if-not (lambda () (or (org-at-keyword-p)
                      (org-at-table-p)
                      (org-at-block-p)))
    ("ms" "Subtree" org-mark-subtree)]

   [""
    :if-not (lambda () (or (org-at-keyword-p)
                      (org-at-table-p)
                      (org-at-block-p)))
    ("me" "Element" org-mark-element)]

   ["Util"
    ("v" "Copy Visible"
     org-copy-visible
     :inapt-if-not (lambda () (use-region-p)))]
   [""
    ("e" "Export…" org-export-dispatch)]])



(transient-define-group causal-org-utility-group
  [
   :if-not (lambda () (if (causal-org-mode-p)
                     (or (org-at-table-p)
                         (org-at-TBLFM-p)
                         (org-at-block-p)
                         (org-at-property-p)
                         (org-at-drawer-p)
                         (org-at-clock-log-p)
                         (org-in-src-block-p)
                         (org-at-keyword-p))
                   t))

   ["Link"
    :inapt-if causal-lib-buffer-read-only-p
    ("l" "Insert…" org-insert-link)
    ("L" "Last" org-insert-last-stored-link)
    ("r" "Cite…" org-cite-insert)]

   ["Timestamp"
    :inapt-if causal-lib-buffer-read-only-p
    ("." "Add…" org-timestamp)
    ("i" "Inactive…" org-timestamp-inactive)]

   ["Clock"
    :pad-keys t
    :inapt-if causal-lib-buffer-read-only-p
    ("M-c" "🕘 in" org-clock-in
     :description (lambda () (causal-org-unicode-get :clock-in))
     :if-not org-clocking-p)
    ("M-c" "🕔 out" org-clock-out
     :description (lambda () (causal-org-unicode-get :clock-out))
     :if org-clocking-p)
    ("R" "🕒 🧾" org-clock-report
     :description (lambda () (causal-org-unicode-get :clock-report))
     :if (lambda () (not (org-at-heading-or-item-p))))]

   ["Display"
    :if causal-org-mode-p
    ("M-i" "Toggle Images" org-toggle-inline-images
     :if (lambda () (and (display-graphic-p) (not (fboundp 'org-link-preview))))
     :transient nil)
    ("M-i" "Toggle Images" causal-org-toggle-images
     :if (lambda () (and (display-graphic-p) (fboundp 'org-link-preview)))
     :transient nil)
    ("M-l" "Link Preview✦" org-link-preview
     :if (lambda () (and (display-graphic-p) (fboundp 'org-link-preview)))
     :transient t)
    ("M" "Show Markup" visible-mode
     :description (lambda () (causal-lib-checkbox-label visible-mode "Show Markup"))
     :transient nil)
    ("P" "Toggle Prettify" prettify-symbols-mode
     :description (lambda () (causal-lib-checkbox-label prettify-symbols-mode
                                                   "Prettify"))
     :transient nil)]

   [""
    :if causal-org-mode-p
    ("V" "Line Wrap" visual-line-mode
     :description (lambda () (causal-lib-checkbox-label visual-line-mode
                                                   "Line Wrap"))
     :transient t)
    ("N" "Number" org-num-mode
     :if org-at-heading-p
     :description (lambda () (causal-lib-checkbox-label org-num-mode
                                                   "Heading #"))
     :transient t)]])


(transient-define-prefix causal-org-table-structure-tmenu ()
  "Menu for Org Table structure (layout) commands."
  :refresh-suffixes t
  :transient-non-suffix t

  ["Org Table Layout"
   :pad-keys t
   :inapt-if-not org-at-table-p
   ["Insert"
    :inapt-if causal-lib-buffer-read-only-p
    ("r" "Row" org-table-insert-row
     :description (lambda () (causal-org-unicode-get :row))
     :transient t)
    ("c" "Column" org-table-insert-column
     :description (lambda () (causal-org-unicode-get :column))
     :transient t)
    ("-" "H Line" org-table-insert-hline :transient t)]

   ["Delete"
    :inapt-if causal-lib-buffer-read-only-p
    ("DEL" "Row" org-table-kill-row
     :description (lambda () (causal-org-unicode-get :row))
     :transient t)
    ("M-DEL" "Column" org-table-delete-column
     :description (lambda () (causal-org-unicode-get :column))
     :transient t)]

   ["Move"
    :inapt-if causal-lib-buffer-read-only-p
    ("M-b" "Column ←" org-table-move-column-left
     :description (lambda () (format "%s %s"
                                (causal-org-unicode-get :column)
                                (causal-org-unicode-get :left)))
     :transient t)]

   [""
    :inapt-if causal-lib-buffer-read-only-p
    ("M-p" "Row ↑" org-table-move-row-up
     :description (lambda () (format "%s %s"
                                (causal-org-unicode-get :row)
                                (causal-org-unicode-get :up)))
     :transient t)
    ("M-n" "Row ↓" org-table-move-row-down
     :description (lambda () (format "%s %s"
                                (causal-org-unicode-get :row)
                                (causal-org-unicode-get :down)))
     :transient t)]

   [""
    :inapt-if causal-lib-buffer-read-only-p
    ("M-f" "Column →" org-table-move-column-right
     :description (lambda () (format "%s %s"
                                (causal-org-unicode-get :column)
                                (causal-org-unicode-get :right)))
     :transient t)]]

  ["Align"
   :class transient-row
   ("al" "Left" causal-org-table-insert-align-left)
   ("ac" "Center" causal-org-table-insert-align-center)
   ("ar" "Right" causal-org-table-insert-align-right)
   ("I" "Width & Alignment" causal-org-table-info-width-alignment
    :description (lambda () (format "%s Width & Align"
                               (causal-org-unicode-get :info))))]

  ["Field"
   [("M-a" "⇤" org-table-beginning-of-field :transient t)]
   [("M-e" "⇥" org-table-end-of-field :transient t)]]

  causal-lib-navigation-group-with-undo-and-return)

(provide 'causal-org-utils)
;;; causal-org-utils.el ends here
