;;; test-causal-xypic.el --- Tests for causal-xypic -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'causal-xypic)

(ert-deftest test-xypic-loads ()
  "causal-xypic feature is present after require."
  (should (featurep 'causal-xypic)))

(ert-deftest test-xypic-in-org-p ()
  "Detects org-mode correctly."
  (with-temp-buffer (org-mode) (should (causal-xypic--in-org-p)))
  (with-temp-buffer (fundamental-mode) (should-not (causal-xypic--in-org-p))))

(ert-deftest test-xypic-insert-wraps-org ()
  "Insertions wrap in \\=\\[ ... \\=\\] in org-mode."
  (with-temp-buffer
    (org-mode)
    (causal-xypic--insert "\\xymatrix{ A & B }")
    (should (string-match-p "\\\\\\[" (buffer-string)))
    (should (string-match-p "\\\\\\]" (buffer-string)))))

(ert-deftest test-xypic-insert-no-wrap-latex ()
  "Insertions do NOT wrap in latex-mode."
  (with-temp-buffer
    (latex-mode)
    (causal-xypic--insert "\\xymatrix{ A & B }")
    (should (string-match-p "xymatrix" (buffer-string)))
    (should-not (string-match-p "\\\\\\[" (buffer-string)))))

(ert-deftest test-xypic-nat-trans ()
  "Natural transformation inserts rtwocell."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'read-string) (lambda (_p &rest _) "X")))
      (causal-xypic-insert-natural-transformation)
      (should (string-match-p "rtwocell" (buffer-string))))))

(ert-deftest test-xypic-vertical-2cell ()
  "Vertical 2-cell inserts dtwocell."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'read-string) (lambda (_p &rest _) "V")))
      (causal-xypic-insert-vertical-2cell)
      (should (string-match-p "dtwocell" (buffer-string))))))

(ert-deftest test-xypic-square-2cell ()
  "Square with 2-cell inserts drtwocell."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'read-string) (lambda (_p &rest _) "S")))
      (causal-xypic-insert-square-2cell)
      (should (string-match-p "drtwocell" (buffer-string))))))

(ert-deftest test-xypic-adjunction ()
  "Adjunction inserts rtwocell with separation."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'read-string) (lambda (_p &rest _) "Q")))
      (causal-xypic-insert-adjunction)
      (let ((s (buffer-string)))
        (should (string-match-p "rtwocell" s))
        (should (string-match-p "<Q>" s))))))

(ert-deftest test-xypic-horiz-comp ()
  "Horizontal composition inserts two rtwocells."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'read-string) (lambda (_p &rest _) "H")))
      (causal-xypic-insert-horizontal-composition)
      (let* ((s (buffer-string))
             (first (string-match "rtwocell" s))
             (second (string-match "rtwocell" s (1+ first))))
        (should first)
        (should second)))))

(ert-deftest test-xypic-comm-square ()
  "Commutative square has horizontal and vertical arrows."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'read-string) (lambda (_p &rest _) "A")))
      (causal-xypic-insert-commutative-square)
      (let ((s (buffer-string)))
        (should (string-match-p "\\\\ar\\[r\\]" s))
        (should (string-match-p "\\\\ar\\[d\\]" s))))))

(ert-deftest test-xypic-triangle ()
  "Triangle has diagonal arrows."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'read-string) (lambda (_p &rest _) "T")))
      (causal-xypic-insert-triangle)
      (let ((s (buffer-string)))
        (should (string-match-p "\\\\ar\\[dl\\]" s))
        (should (string-match-p "\\\\ar\\[dr\\]" s))
        (should (string-match-p "\\\\ar\\[rr\\]" s))))))

(ert-deftest test-xypic-raw-grid ()
  "Raw grid creates xymatrix with correct dimensions."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'read-number) (lambda (_p &rest _) 3)))
      (causal-xypic-insert-raw)
      (let ((s (buffer-string)))
        (should (string-match-p "xymatrix" s))
        ;; 3x3 grid: 3 rows with & separators, 2 row-break \\\\ sequences
        (should (string-match-p "&" s))
        (should (string-match-p "\\\\bullet" s))))))

(ert-deftest test-xypic-default-column-sep ()
  "Default column separation appears in output."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'read-string) (lambda (_p &rest _) "X")))
      (causal-xypic-insert-natural-transformation)
      (should (string-match-p "4pc" (buffer-string))))))

(ert-deftest test-xypic-custom-column-sep ()
  "Custom column separation is respected."
  (with-temp-buffer
    (org-mode)
    (let ((causal-xypic-default-column-sep "6pc"))
      (cl-letf (((symbol-function 'read-string) (lambda (_p &rest _) "X")))
        (causal-xypic-insert-natural-transformation)
        (should (string-match-p "6pc" (buffer-string)))))))

(ert-deftest test-xypic-wrap-org-nil ()
  "When wrap-org is nil, org-mode insertions are not wrapped."
  (with-temp-buffer
    (org-mode)
    (let ((causal-xypic-wrap-org nil))
      (causal-xypic--insert "\\xymatrix{ A & B }")
      (should-not (string-match-p "\\\\\\[" (buffer-string))))))

(provide 'test-causal-xypic)
;;; test-causal-xypic.el ends here
