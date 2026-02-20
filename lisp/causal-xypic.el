;;; causal-xypic.el --- Transient UI for XY-pic 2-Cell Diagrams -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, tex, categories
;; URL: https://github.com/plurigrid/causal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Transient-based porcelain for inserting XY-pic 2-cell diagrams
;; directly into org-mode buffers or LaTeX files via AUCTeX.
;;
;; 2-cells are the natural transformations, modifications, and higher
;; morphisms that live between 1-cells (functors/arrows).  XY-pic's
;; `2cell' extension provides \rtwocell, \dtwocell, \drtwocell etc.
;;
;; Works in:
;;   - org-mode: inserts \[ ... \] LaTeX fragments (preview with C-c C-x C-l)
;;   - LaTeX-mode (AUCTeX): inserts raw XY-pic
;;
;; Requires: \usepackage[all,2cell]{xy} \UseAllTwocells in preamble.
;;
;; INSTALLATION
;;   (require 'causal-xypic)
;;   (keymap-global-set "C-c x" #'causal-xypic-tmenu)
;;
;; Or from causal-proof:
;;   The menu integrates via the "Diagrams" section.

;;; Code:
(require 'transient)
(require 'causal-lib)

(defgroup causal-xypic nil
  "Settings for XY-pic 2-cell diagram insertion."
  :group 'causal)

(defcustom causal-xypic-default-column-sep "4pc"
  "Default column separation for xymatrix."
  :type 'string
  :group 'causal-xypic)

(defcustom causal-xypic-wrap-org t
  "If non-nil, wrap insertions in \\=\\[ ... \\=\\] when in `org-mode'."
  :type 'boolean
  :group 'causal-xypic)

;;; ── Helpers ──────────────────────────────────────────────────────────

(defun causal-xypic--in-org-p ()
  "Return non-nil if current buffer is org-mode."
  (derived-mode-p 'org-mode))

(defun causal-xypic--insert (body)
  "Insert XY-pic BODY, wrapping for org-mode if needed."
  (let ((text (if (and causal-xypic-wrap-org (causal-xypic--in-org-p))
                  (format "\\[\n%s\n\\]" body)
                body)))
    (insert text)
    (when (causal-xypic--in-org-p)
      (message "Preview: C-c C-x C-l  |  Export PDF: C-c C-e l p"))))

(defun causal-xypic--read-labels (prompt count)
  "Read COUNT labels from user with PROMPT prefix.
Returns a list of strings."
  (cl-loop for i from 1 to count
           collect (read-string (format "%s %d: " prompt i))))

;;; ── 2-Cell Templates ─────────────────────────────────────────────────

(defun causal-xypic-insert-natural-transformation ()
  "Insert horizontal 2-cell: A ⇒ B (natural transformation α: F ⇒ G)."
  (interactive)
  (let ((src (read-string "Source object [A]: " nil nil "A"))
        (tgt (read-string "Target object [B]: " nil nil "B"))
        (top (read-string "Top functor [F]: " nil nil "F"))
        (bot (read-string "Bottom functor [G]: " nil nil "G"))
        (cell (read-string "2-cell label [\\alpha]: " nil nil "\\alpha")))
    (causal-xypic--insert
     (format "\\xymatrix@C=%s{\n  %s \\rtwocell^{%s}_{%s}{%s} & %s\n}"
             causal-xypic-default-column-sep src top bot cell tgt))))

(defun causal-xypic-insert-vertical-2cell ()
  "Insert vertical 2-cell: A ⇓ B."
  (interactive)
  (let ((src (read-string "Top object [A]: " nil nil "A"))
        (tgt (read-string "Bottom object [B]: " nil nil "B"))
        (left (read-string "Left arrow [F]: " nil nil "F"))
        (right (read-string "Right arrow [G]: " nil nil "G"))
        (cell (read-string "2-cell label [\\alpha]: " nil nil "\\alpha")))
    (causal-xypic--insert
     (format "\\xymatrix{\n  %s \\ar@/^1pc/[d]^{%s} \\ar@/_1pc/[d]_{%s} \\dtwocell{%s} \\\\\n  %s\n}"
             src left right cell tgt))))

(defun causal-xypic-insert-square-2cell ()
  "Insert commutative square with 2-cell filling."
  (interactive)
  (let ((a (read-string "Top-left [A]: " nil nil "A"))
        (b (read-string "Top-right [B]: " nil nil "B"))
        (c (read-string "Bottom-left [C]: " nil nil "C"))
        (d (read-string "Bottom-right [D]: " nil nil "D"))
        (f (read-string "Top arrow [f]: " nil nil "f"))
        (g (read-string "Bottom arrow [g]: " nil nil "g"))
        (h (read-string "Left arrow [h]: " nil nil "h"))
        (k (read-string "Right arrow [k]: " nil nil "k"))
        (cell (read-string "2-cell label [\\alpha]: " nil nil "\\alpha")))
    (causal-xypic--insert
     (format "\\xymatrix{\n  %s \\ar[r]^{%s} \\ar[d]_{%s} \\drtwocell{%s} & %s \\ar[d]^{%s} \\\\\n  %s \\ar[r]_{%s} & %s\n}"
             a f h cell b k c g d))))

(defun causal-xypic-insert-adjunction ()
  "Insert adjunction 2-cell (unit η or counit ε)."
  (interactive)
  (let ((src (read-string "Left category [\\mathcal{C}]: " nil nil "\\mathcal{C}"))
        (tgt (read-string "Right category [\\mathcal{D}]: " nil nil "\\mathcal{D}"))
        (left (read-string "Left adjoint [F]: " nil nil "F"))
        (right (read-string "Right adjoint [G]: " nil nil "G"))
        (cell (read-string "Unit/counit [\\eta]: " nil nil "\\eta"))
        (sep (read-string "Separation [5]: " nil nil "5")))
    (causal-xypic--insert
     (format "\\xymatrix@C=%s{\n  %s \\rtwocell<%s>^{%s}_{%s}{%s} & %s\n}"
             causal-xypic-default-column-sep src sep left right cell tgt))))

(defun causal-xypic-insert-horizontal-composition ()
  "Insert horizontal composition of two 2-cells (Godement product)."
  (interactive)
  (let ((a (read-string "Object A [A]: " nil nil "A"))
        (b (read-string "Object B [B]: " nil nil "B"))
        (c (read-string "Object C [C]: " nil nil "C"))
        (f (read-string "Top-left [F]: " nil nil "F"))
        (g (read-string "Bot-left [G]: " nil nil "G"))
        (h (read-string "Top-right [H]: " nil nil "H"))
        (k (read-string "Bot-right [K]: " nil nil "K"))
        (alpha (read-string "Left 2-cell [\\alpha]: " nil nil "\\alpha"))
        (beta (read-string "Right 2-cell [\\beta]: " nil nil "\\beta")))
    (causal-xypic--insert
     (format "\\xymatrix{\n  %s \\rtwocell^{%s}_{%s}{%s} & %s \\rtwocell^{%s}_{%s}{%s} & %s\n}"
             a f g alpha b h k beta c))))

(defun causal-xypic-insert-commutative-square ()
  "Insert standard commutative square (1-cells only, no 2-cell)."
  (interactive)
  (let ((a (read-string "Top-left [X]: " nil nil "X"))
        (b (read-string "Top-right [Y]: " nil nil "Y"))
        (c (read-string "Bottom-left [Z]: " nil nil "Z"))
        (d (read-string "Bottom-right [W]: " nil nil "W"))
        (f (read-string "Top [f]: " nil nil "f"))
        (g (read-string "Bottom [g]: " nil nil "g"))
        (h (read-string "Left [h]: " nil nil "h"))
        (k (read-string "Right [k]: " nil nil "k")))
    (causal-xypic--insert
     (format "\\xymatrix{\n  %s \\ar[r]^{%s} \\ar[d]_{%s} & %s \\ar[d]^{%s} \\\\\n  %s \\ar[r]_{%s} & %s\n}"
             a f h b k c g d))))

(defun causal-xypic-insert-triangle ()
  "Insert a triangle diagram."
  (interactive)
  (let ((a (read-string "Top [A]: " nil nil "A"))
        (b (read-string "Bottom-left [B]: " nil nil "B"))
        (c (read-string "Bottom-right [C]: " nil nil "C"))
        (f (read-string "Left arrow [f]: " nil nil "f"))
        (g (read-string "Right arrow [g]: " nil nil "g"))
        (h (read-string "Bottom arrow [h]: " nil nil "h")))
    (causal-xypic--insert
     (format "\\xymatrix{\n  & %s \\ar[dl]_{%s} \\ar[dr]^{%s} & \\\\\n  %s \\ar[rr]_{%s} & & %s\n}"
             a f g b h c))))

(defun causal-xypic-insert-raw ()
  "Insert a raw \\xymatrix{} skeleton for freeform editing."
  (interactive)
  (let ((rows (read-number "Rows: " 2))
        (cols (read-number "Columns: " 2)))
    (causal-xypic--insert
     (concat "\\xymatrix{\n"
             (mapconcat
              (lambda (r)
                (concat "  "
                        (mapconcat (lambda (c)
                                     (format "\\bullet" ))
                                   (number-sequence 1 cols)
                                   " & ")))
              (number-sequence 1 rows)
              " \\\\\n")
             "\n}"))))

;;; ── Preview & Export ─────────────────────────────────────────────────

(defun causal-xypic-preview-at-point ()
  "Preview the LaTeX fragment at point in org-mode."
  (interactive)
  (if (causal-xypic--in-org-p)
      (org-latex-preview)
    (if (fboundp 'preview-at-point)
        (preview-at-point)
      (message "Preview requires org-mode or AUCTeX preview-latex."))))

(defun causal-xypic-preview-buffer ()
  "Preview all LaTeX fragments in the buffer."
  (interactive)
  (if (causal-xypic--in-org-p)
      (org-latex-preview '(16))
    (if (fboundp 'preview-buffer)
        (preview-buffer)
      (message "Preview requires org-mode or AUCTeX preview-latex."))))

(defun causal-xypic-export-pdf ()
  "Export current org buffer to PDF."
  (interactive)
  (if (causal-xypic--in-org-p)
      (org-latex-export-to-pdf)
    (if (fboundp 'TeX-command-master)
        (TeX-command-master)
      (message "Export requires org-mode or AUCTeX."))))

;;; ── Transient Menus ──────────────────────────────────────────────────

(transient-define-prefix causal-xypic-2cell-tmenu ()
  "Insert 2-cell diagrams."
  ["2-Cells (natural transformations & higher)"
   ("n" "α: F ⇒ G (horizontal)" causal-xypic-insert-natural-transformation)
   ("v" "α: F ⇓ G (vertical)" causal-xypic-insert-vertical-2cell)
   ("s" "□ with 2-cell fill" causal-xypic-insert-square-2cell)
   ("a" "F ⊣ G adjunction" causal-xypic-insert-adjunction)
   ("h" "α∘β horizontal comp" causal-xypic-insert-horizontal-composition)]
  [:class transient-row
   (causal-lib-quit-one)])

(transient-define-prefix causal-xypic-1cell-tmenu ()
  "Insert 1-cell (arrow) diagrams."
  ["1-Cells (commutative diagrams)"
   ("s" "Commutative square" causal-xypic-insert-commutative-square)
   ("t" "Triangle" causal-xypic-insert-triangle)
   ("r" "Raw xymatrix grid" causal-xypic-insert-raw)]
  [:class transient-row
   (causal-lib-quit-one)])

(transient-define-prefix causal-xypic-preview-tmenu ()
  "Preview & export."
  ["Preview"
   ("p" "Preview at point" causal-xypic-preview-at-point)
   ("b" "Preview buffer" causal-xypic-preview-buffer)]
  ["Export"
   ("e" "Export PDF" causal-xypic-export-pdf)]
  [:class transient-row
   (causal-lib-quit-one)])

;;;###autoload (autoload 'causal-xypic-tmenu "causal-xypic" nil t)

(transient-define-prefix causal-xypic-tmenu ()
  "Causal XY-pic — 2-cell diagram insertion for org-mode & AUCTeX."

  [:description
   (lambda ()
     (format "XY-pic [%s]"
             (cond ((derived-mode-p 'org-mode) "org")
                   ((derived-mode-p 'latex-mode) "LaTeX")
                   (t (symbol-name major-mode)))))]

  [["2-Cells"
    ("2" "2-Cell diagrams›" causal-xypic-2cell-tmenu)
    ("n" "Quick: α: F ⇒ G" causal-xypic-insert-natural-transformation)]

   ["1-Cells"
    ("1" "1-Cell diagrams›" causal-xypic-1cell-tmenu)
    ("s" "Quick: □ square" causal-xypic-insert-commutative-square)]

   ["Preview"
    ("p" "Preview›" causal-xypic-preview-tmenu)
    ("P" "Preview at point" causal-xypic-preview-at-point
     :transient t)]]

  [:class transient-row
   (causal-lib-quit-one)
   (causal-lib-quit-all)])

(provide 'causal-xypic)
;;; causal-xypic.el ends here
