;;; test-causal-narya-canonical.el --- Tests for causal-narya-canonical -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(require 'causal-narya-canonical)

;; --- Feature loading ---

(ert-deftest test-narya-canonical-loads ()
  "Feature is present after require."
  (should (featurep 'causal-narya-canonical)))

(ert-deftest test-narya-canonical-schemata-defined ()
  "Customizable schemata variable is a non-empty list."
  (should (listp causal-narya-canonical-schemata))
  (should (> (length causal-narya-canonical-schemata) 0)))

;; --- causal-narya-canonical-all ---

(ert-deftest test-narya-canonical-all-returns-list ()
  "all returns a list of plists."
  (let ((all (causal-narya-canonical-all)))
    (should (listp all))
    (should (> (length all) 0))
    (should (plist-get (car all) :id))))

(ert-deftest test-narya-canonical-all-sorted-by-awareness ()
  "all returns schemata sorted by ascending awareness."
  (let* ((all (causal-narya-canonical-all))
         (levels (mapcar (lambda (s) (plist-get s :awareness)) all)))
    (should (equal levels (sort (copy-sequence levels) #'<)))))

(ert-deftest test-narya-canonical-all-has-six-schemata ()
  "Default schemata has exactly 6 entries (awareness 0-5)."
  (should (= (length (causal-narya-canonical-all)) 6)))

(ert-deftest test-narya-canonical-all-unique-ids ()
  "All schema ids are unique."
  (let* ((all (causal-narya-canonical-all))
         (ids (mapcar (lambda (s) (plist-get s :id)) all)))
    (should (= (length ids) (length (cl-remove-duplicates ids))))))

;; --- causal-narya-canonical-find ---

(ert-deftest test-narya-canonical-find-homoiconic ()
  "find returns the homoiconic-syntax schema."
  (let ((schema (causal-narya-canonical-find 'homoiconic-syntax)))
    (should schema)
    (should (eq (plist-get schema :id) 'homoiconic-syntax))
    (should (= (plist-get schema :awareness) 0))))

(ert-deftest test-narya-canonical-find-all-known ()
  "find returns non-nil for all known schema ids."
  (dolist (id '(homoiconic-syntax structural-recursion lexical-scope
                macro-non-leakage replay-determinism proof-state-olog))
    (should (causal-narya-canonical-find id))))

(ert-deftest test-narya-canonical-find-unknown-returns-nil ()
  "find returns nil for unknown schema id."
  (should-not (causal-narya-canonical-find 'nonexistent-schema)))

;; --- causal-narya-canonical-for-awareness ---

(ert-deftest test-narya-canonical-for-awareness-0 ()
  "for-awareness 0 returns only the awareness-0 schema."
  (let ((filtered (causal-narya-canonical-for-awareness 0)))
    (should (= (length filtered) 1))
    (should (= (plist-get (car filtered) :awareness) 0))))

(ert-deftest test-narya-canonical-for-awareness-5 ()
  "for-awareness 5 returns all schemata."
  (let ((filtered (causal-narya-canonical-for-awareness 5)))
    (should (= (length filtered) 6))))

(ert-deftest test-narya-canonical-for-awareness-3 ()
  "for-awareness 3 returns schemata 0 through 3."
  (let ((filtered (causal-narya-canonical-for-awareness 3)))
    (should (= (length filtered) 4))
    (dolist (s filtered)
      (should (<= (plist-get s :awareness) 3)))))

;; --- causal-narya-canonical-proof-bundle ---

(ert-deftest test-narya-canonical-proof-bundle-returns-alist ()
  "proof-bundle returns a well-formed alist."
  (let ((bundle (causal-narya-canonical-proof-bundle 'homoiconic-syntax)))
    (should bundle)
    (should (assoc 'id bundle))
    (should (assoc 'title bundle))
    (should (assoc 'awareness bundle))
    (should (assoc 'claim bundle))
    (should (assoc 'proof_kind bundle))
    (should (assoc 'invariants bundle))
    (should (assoc 'before bundle))
    (should (assoc 'after bundle))
    (should (assoc 'delta bundle))
    (should (assoc 'birth bundle))
    (should (assoc 'impact bundle))))

(ert-deftest test-narya-canonical-proof-bundle-values ()
  "proof-bundle values are strings (serializable)."
  (let ((bundle (causal-narya-canonical-proof-bundle 'lexical-scope)))
    (should (stringp (cdr (assoc 'id bundle))))
    (should (stringp (cdr (assoc 'title bundle))))
    (should (numberp (cdr (assoc 'awareness bundle))))
    (should (listp (cdr (assoc 'invariants bundle))))
    (dolist (inv (cdr (assoc 'invariants bundle)))
      (should (stringp inv)))))

(ert-deftest test-narya-canonical-proof-bundle-unknown-nil ()
  "proof-bundle returns nil for unknown schema."
  (should-not (causal-narya-canonical-proof-bundle 'nonexistent)))

;; --- causal-narya-canonical-olog ---

(ert-deftest test-narya-canonical-olog-structure ()
  "olog returns objects and aspects."
  (let ((olog (causal-narya-canonical-olog 'structural-recursion)))
    (should olog)
    (should (assoc 'objects olog))
    (should (assoc 'aspects olog))
    (let ((objects (cdr (assoc 'objects olog))))
      (should (assoc 'source objects))
      (should (assoc 'target objects))
      (should (assoc 'witness objects)))
    (let ((aspects (cdr (assoc 'aspects olog))))
      (should (assoc 'local_to_global aspects))
      (should (assoc 'birth aspects))
      (should (assoc 'impact aspects)))))

(ert-deftest test-narya-canonical-olog-unknown-nil ()
  "olog returns nil for unknown schema."
  (should-not (causal-narya-canonical-olog 'nonexistent)))

;; --- causal-narya-canonical-current-world ---

(ert-deftest test-narya-canonical-current-world-has-backend ()
  "current-world includes a backend field."
  (let ((world (causal-narya-canonical-current-world 'homoiconic-syntax)))
    (should world)
    (should (assoc 'backend world))))

(ert-deftest test-narya-canonical-current-world-unknown-nil ()
  "current-world returns nil for unknown schema."
  (should-not (causal-narya-canonical-current-world 'nonexistent)))

;; --- Schema field completeness ---

(ert-deftest test-narya-canonical-all-have-required-fields ()
  "Every schema has all required Narya proof bundle fields."
  (dolist (schema (causal-narya-canonical-all))
    (dolist (key '(:id :awareness :title :claim :canonical-lisp-fact
                   :proof-kind :invariants :before :after :delta :birth
                   :impact :topos-note))
      (should (plist-get schema key)))))

;; --- format ---

(ert-deftest test-narya-canonical-format-schema ()
  "Format produces a non-empty string."
  (let* ((schema (causal-narya-canonical-find 'replay-determinism))
         (formatted (causal-narya-canonical--format-schema schema)))
    (should (stringp formatted))
    (should (> (length formatted) 100))
    (should (string-match-p "replay" (downcase formatted)))))

;; --- read-schema (non-interactive) ---

(ert-deftest test-narya-canonical-sort-idempotent ()
  "Sorting an already-sorted list is idempotent."
  (let* ((all (causal-narya-canonical-all))
         (sorted-again (causal-narya-canonical--sort all)))
    (should (equal (mapcar (lambda (s) (plist-get s :id)) all)
                   (mapcar (lambda (s) (plist-get s :id)) sorted-again)))))

(provide 'test-causal-narya-canonical)
;;; test-causal-narya-canonical.el ends here
