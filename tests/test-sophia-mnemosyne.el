;;; test-sophia-mnemosyne.el --- Outside-in tests for Mnemosyne -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Outside-in ERT tests for sophia-mnemosyne.el.
;; Layers tested from periphery → core:
;;   1. User commands (interactive surface)
;;   2. Buffer/message output (observable effects)
;;   3. API request formation (wire format)
;;   4. Customization defaults (configuration)
;;
;; All tests mock `sophia-mnemosyne--api' — no running backend needed.
;; SDF Ch.8 degeneracy: the mock IS a fallback strategy.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sophia-mnemosyne)

;;; ─── Test Infrastructure ───────────────────────────────────────────

(defvar test-sm--api-calls nil
  "Accumulator: each API call pushes (METHOD ENDPOINT BODY).")

(defvar test-sm--api-response nil
  "Canned response for the next `sophia-mnemosyne--api' call.")

(defmacro test-sm-with-mock (response &rest body)
  "Execute BODY with API mocked to return RESPONSE.
Captures all API calls in `test-sm--api-calls'."
  (declare (indent 1))
  `(let ((test-sm--api-calls nil)
         (test-sm--api-response ,response))
     (cl-letf (((symbol-function 'sophia-mnemosyne--api)
                (lambda (method endpoint &optional body)
                  (push (list method endpoint body) test-sm--api-calls)
                  test-sm--api-response)))
       ,@body)))

(defun test-sm--last-call ()
  "Return the most recent (METHOD ENDPOINT BODY) triple."
  (car test-sm--api-calls))

(defun test-sm--last-message ()
  "Capture the last `message' output."
  (let ((msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (funcall (lambda () msg)))))

;;; ─── Layer 1: Customization Defaults ───────────────────────────────

(ert-deftest test-sm-default-url ()
  "Default URL points to localhost:8001."
  (should (string= "http://127.0.0.1:8001" sophia-mnemosyne-url)))

(ert-deftest test-sm-default-token ()
  "Default dev token is 'dev-local'."
  (should (string= "dev-local" sophia-mnemosyne-dev-token)))

(ert-deftest test-sm-customization-group ()
  "sophia-mnemosyne customization group exists."
  (should (get 'sophia-mnemosyne 'custom-group)))

;;; ─── Layer 2: API Wire Format ──────────────────────────────────────

(ert-deftest test-sm-api-graphs-get ()
  "list-graphs sends GET /graphs."
  (test-sm-with-mock [((name . "test-graph"))]
    (sophia-mnemosyne-list-graphs)
    (let ((call (test-sm--last-call)))
      (should (string= "GET" (nth 0 call)))
      (should (string= "/graphs" (nth 1 call)))
      (should (null (nth 2 call))))))

(ert-deftest test-sm-api-create-graph-post ()
  "create-graph sends POST /graphs with name."
  (test-sm-with-mock '((id . "g1"))
    (sophia-mnemosyne-create-graph "my-graph")
    (let ((call (test-sm--last-call)))
      (should (string= "POST" (nth 0 call)))
      (should (string= "/graphs" (nth 1 call)))
      (should (equal '((name . "my-graph")) (nth 2 call))))))

(ert-deftest test-sm-api-sparql-post ()
  "sparql-query sends POST /sparql with query body."
  (test-sm-with-mock '((results . []))
    (sophia-mnemosyne-sparql-query "SELECT ?s WHERE { ?s ?p ?o }")
    (let ((call (test-sm--last-call)))
      (should (string= "POST" (nth 0 call)))
      (should (string= "/sparql" (nth 1 call)))
      (should (equal '((query . "SELECT ?s WHERE { ?s ?p ?o }"))
                     (nth 2 call))))))

(ert-deftest test-sm-api-write-document-post ()
  "write-document sends POST /documents with name+content."
  (test-sm-with-mock '((ok . t))
    (sophia-mnemosyne-write-document "proof.ny" "def foo : Nat := 42")
    (let ((call (test-sm--last-call)))
      (should (string= "POST" (nth 0 call)))
      (should (string= "/documents" (nth 1 call)))
      (should (equal '((name . "proof.ny") (content . "def foo : Nat := 42"))
                     (nth 2 call))))))

(ert-deftest test-sm-api-read-document-get ()
  "read-document sends GET /documents/<url-encoded-name>."
  (test-sm-with-mock '((content . "hello"))
    (sophia-mnemosyne-read-document "my doc")
    (let ((call (test-sm--last-call)))
      (should (string= "GET" (nth 0 call)))
      (should (string= "/documents/my%20doc" (nth 1 call))))))

(ert-deftest test-sm-api-create-wire-post ()
  "create-wire sends POST /wires with from/to/predicate."
  (test-sm-with-mock '((ok . t))
    (sophia-mnemosyne-create-wire "lemma-1" "thm-2" "dependsOn")
    (let ((call (test-sm--last-call)))
      (should (string= "POST" (nth 0 call)))
      (should (string= "/wires" (nth 1 call)))
      (should (equal '((from . "lemma-1") (to . "thm-2") (predicate . "dependsOn"))
                     (nth 2 call))))))

(ert-deftest test-sm-api-traverse-wires-get ()
  "traverse-wires sends GET /wires/<url-encoded-id>."
  (test-sm-with-mock [((to . "b") (predicate . "uses"))]
    (sophia-mnemosyne-traverse-wires "node/1")
    (let ((call (test-sm--last-call)))
      (should (string= "GET" (nth 0 call)))
      (should (string= "/wires/node%2F1" (nth 1 call))))))

(ert-deftest test-sm-api-health-get ()
  "status sends GET /health."
  (test-sm-with-mock '((status . "ok"))
    (sophia-mnemosyne-status)
    (let ((call (test-sm--last-call)))
      (should (string= "GET" (nth 0 call)))
      (should (string= "/health" (nth 1 call))))))

;;; ─── Layer 3: Buffer Output ────────────────────────────────────────

(ert-deftest test-sm-list-graphs-buffer ()
  "list-graphs creates *Mnemosyne Graphs* with graph names."
  (test-sm-with-mock [((name . "alpha")) ((name . "beta"))]
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (sophia-mnemosyne-list-graphs)
      (let ((buf (get-buffer "*Mnemosyne Graphs*")))
        (should buf)
        (with-current-buffer buf
          (should (string-match-p "alpha" (buffer-string)))
          (should (string-match-p "beta" (buffer-string))))
        (kill-buffer buf)))))

(ert-deftest test-sm-list-graphs-fallback-id ()
  "list-graphs falls back to 'id' when 'name' is missing."
  (test-sm-with-mock [((id . "g-001"))]
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (sophia-mnemosyne-list-graphs)
      (let ((buf (get-buffer "*Mnemosyne Graphs*")))
        (should buf)
        (with-current-buffer buf
          (should (string-match-p "g-001" (buffer-string))))
        (kill-buffer buf)))))

(ert-deftest test-sm-sparql-buffer ()
  "sparql-query creates *SPARQL Results* buffer."
  (test-sm-with-mock '((results . [((s . "http://ex.org/foo"))]))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (sophia-mnemosyne-sparql-query "SELECT ?s WHERE { ?s ?p ?o }")
      (let ((buf (get-buffer "*SPARQL Results*")))
        (should buf)
        (with-current-buffer buf
          (should (string-match-p "ex.org/foo" (buffer-string))))
        (kill-buffer buf)))))

(ert-deftest test-sm-read-document-buffer ()
  "read-document creates *Mnemosyne: <name>* buffer with content."
  (test-sm-with-mock '((content . "theorem nat_succ : ..."))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (sophia-mnemosyne-read-document "proof1")
      (let ((buf (get-buffer "*Mnemosyne: proof1*")))
        (should buf)
        (with-current-buffer buf
          (should (string-match-p "nat_succ" (buffer-string))))
        (kill-buffer buf)))))

(ert-deftest test-sm-traverse-wires-buffer ()
  "traverse-wires creates *Mnemosyne Wires* buffer."
  (test-sm-with-mock [((to . "thm-1") (predicate . "proves"))]
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (sophia-mnemosyne-traverse-wires "lemma-x")
      (let ((buf (get-buffer "*Mnemosyne Wires*")))
        (should buf)
        (with-current-buffer buf
          (should (string-match-p "lemma-x" (buffer-string)))
          (should (string-match-p "proves" (buffer-string))))
        (kill-buffer buf)))))

;;; ─── Layer 4: Message Output ───────────────────────────────────────

(ert-deftest test-sm-create-graph-message ()
  "create-graph messages success on non-nil response."
  (let ((msg nil))
    (test-sm-with-mock '((id . "g1"))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (sophia-mnemosyne-create-graph "test")))
    (should (string-match-p "test.*created" msg))))

(ert-deftest test-sm-create-graph-failure-message ()
  "create-graph messages failure on nil response."
  (let ((msg nil))
    (test-sm-with-mock nil
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (sophia-mnemosyne-create-graph "test")))
    (should (string-match-p "Failed" msg))))

(ert-deftest test-sm-write-document-message ()
  "write-document messages success."
  (let ((msg nil))
    (test-sm-with-mock '((ok . t))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (sophia-mnemosyne-write-document "x.ny" "content")))
    (should (string-match-p "x\\.ny.*saved" msg))))

(ert-deftest test-sm-create-wire-message ()
  "create-wire messages the triple on success."
  (let ((msg nil))
    (test-sm-with-mock '((ok . t))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (sophia-mnemosyne-create-wire "a" "b" "links")))
    (should (string-match-p "a" msg))
    (should (string-match-p "links" msg))
    (should (string-match-p "b" msg))))

(ert-deftest test-sm-status-connected ()
  "status messages 'connected' on non-nil response."
  (let ((msg nil))
    (test-sm-with-mock '((status . "ok"))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (sophia-mnemosyne-status)))
    (should (string-match-p "connected" msg))))

(ert-deftest test-sm-status-unreachable ()
  "status messages 'no response' on nil API return."
  (let ((msg nil))
    (test-sm-with-mock nil
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (sophia-mnemosyne-status)))
    (should (string-match-p "no response" msg))))

;;; ─── Layer 5: Save-buffer Integration ──────────────────────────────

(ert-deftest test-sm-save-buffer-uses-buffer-name ()
  "save-buffer derives document name from buffer name."
  (test-sm-with-mock '((ok . t))
    (with-temp-buffer
      (rename-buffer "test-proof.ny" t)
      (insert "def hello := 1")
      (sophia-mnemosyne-save-buffer)
      (let ((call (test-sm--last-call)))
        (should (string= "POST" (nth 0 call)))
        (should (string= "/documents" (nth 1 call)))
        (should (string= "test-proof.ny" (alist-get 'name (nth 2 call))))
        (should (string= "def hello := 1" (alist-get 'content (nth 2 call))))))))

(ert-deftest test-sm-save-buffer-uses-file-name ()
  "save-buffer derives document name from file-name when visiting."
  (test-sm-with-mock '((ok . t))
    (let ((temp-file (make-temp-file "sm-test-" nil ".ny")))
      (unwind-protect
          (progn
            (with-current-buffer (find-file-noselect temp-file)
              (insert "def world := 2")
              (sophia-mnemosyne-save-buffer)
              (let* ((call (test-sm--last-call))
                     (doc-name (alist-get 'name (nth 2 call))))
                (should (string-match-p "\\.ny$" doc-name)))
              (kill-buffer)))
        (delete-file temp-file)))))

;;; ─── Layer 6: No-backend Graceful Degradation ──────────────────────

(ert-deftest test-sm-list-graphs-nil-messages ()
  "list-graphs messages when API returns nil."
  (let ((msg nil))
    (test-sm-with-mock nil
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (sophia-mnemosyne-list-graphs)))
    (should (string-match-p "unreachable\\|No graphs" msg))))

(ert-deftest test-sm-sparql-nil-no-buffer ()
  "sparql-query does not create buffer when API returns nil."
  (let ((buf-before (get-buffer "*SPARQL Results*")))
    (when buf-before (kill-buffer buf-before))
    (test-sm-with-mock nil
      (sophia-mnemosyne-sparql-query "SELECT 1"))
    (should-not (get-buffer "*SPARQL Results*"))))

(ert-deftest test-sm-read-document-nil-no-buffer ()
  "read-document does not create buffer when API returns nil."
  (let ((buf-before (get-buffer "*Mnemosyne: gone*")))
    (when buf-before (kill-buffer buf-before))
    (test-sm-with-mock nil
      (sophia-mnemosyne-read-document "gone"))
    (should-not (get-buffer "*Mnemosyne: gone*"))))

(ert-deftest test-sm-traverse-wires-nil-no-buffer ()
  "traverse-wires does not create buffer when API returns nil."
  (let ((buf-before (get-buffer "*Mnemosyne Wires*")))
    (when buf-before (kill-buffer buf-before))
    (test-sm-with-mock nil
      (sophia-mnemosyne-traverse-wires "nope"))
    (should-not (get-buffer "*Mnemosyne Wires*"))))

(provide 'test-sophia-mnemosyne)
;;; test-sophia-mnemosyne.el ends here
