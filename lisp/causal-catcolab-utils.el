;;; causal-catcolab-utils.el --- CatColab RPC utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, categories
;; URL: https://github.com/plurigrid/causal

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

;; JSON-RPC 2.0 client for the CatColab backend.
;;
;; Wire format (confirmed against backend.catcolab.org):
;;   POST <url>/rpc
;;   Content-Type: application/json
;;   Body: {"jsonrpc":"2.0","method":"<name>","params":[<args>],"id":<n>}
;;
;; Response:
;;   {"jsonrpc":"2.0","result":{"tag":"Ok","content":<data>},"id":<n>}
;;   {"jsonrpc":"2.0","result":{"tag":"Err","code":<n>,"message":"..."},"id":<n>}

;;; Code:

(require 'url)
(require 'json)

;;; Customization

(defgroup causal-catcolab nil
  "CatColab integration for Causal."
  :group 'tools
  :prefix "causal-catcolab-")

(defcustom causal-catcolab-url "https://backend.catcolab.org"
  "CatColab backend URL.
Use \"http://localhost:8000\" for a local development instance."
  :type 'string
  :group 'causal-catcolab)

(defcustom causal-catcolab-token nil
  "Firebase Bearer token for authenticated CatColab requests.
When nil, only public documents are accessible."
  :type '(choice (const :tag "Anonymous (public only)" nil) string)
  :group 'causal-catcolab)

;;; JSON-RPC 2.0 Client

(defvar causal-catcolab--rpc-id 0
  "Auto-incrementing JSON-RPC request ID.")

(defun causal-catcolab--next-id ()
  "Return the next JSON-RPC request ID."
  (cl-incf causal-catcolab--rpc-id))

(defun causal-catcolab--request (method &rest params)
  "Call CatColab RPC METHOD with PARAMS via JSON-RPC 2.0.

Returns the `content' field on Ok, or signals an error on Err."
  (let* ((id (causal-catcolab--next-id))
         (body (json-encode
                `(("jsonrpc" . "2.0")
                  ("method" . ,method)
                  ("params" . ,params)
                  ("id" . ,id))))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ,@(when causal-catcolab-token
                `(("Authorization" . ,(concat "Bearer " causal-catcolab-token))))))
         (url-request-data (encode-coding-string body 'utf-8))
         (endpoint (concat causal-catcolab-url "/rpc")))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously endpoint t t 15)
          (goto-char url-http-end-of-headers)
          (let* ((json-object-type 'alist)
                 (json-array-type 'vector)
                 (response (json-read))
                 (result (alist-get 'result response)))
            (pcase (alist-get 'tag result)
              ("Ok" (alist-get 'content result))
              ("Err"
               (user-error "CatColab RPC error %s: %s"
                           (alist-get 'code result)
                           (alist-get 'message result)))
              (_ (user-error "CatColab unexpected response: %S" result)))))
      (error
       (user-error "CatColab request failed: %s" (error-message-string err))))))

;;; Typed Query Helpers

(defun causal-catcolab-search-refs (&optional name-query limit)
  "Search CatColab refs matching NAME-QUERY (string or nil), up to LIMIT results."
  (causal-catcolab--request
   "search_ref_stubs"
   `(("ownerUsernameQuery" . nil)
     ("refNameQuery" . ,name-query)
     ("searcherMinLevel" . nil)
     ("includePublicDocuments" . t)
     ("onlyDeleted" . nil)
     ("limit" . ,(or limit 20))
     ("offset" . nil))))

(defun causal-catcolab-get-doc (ref-id)
  "Get document metadata for REF-ID."
  (causal-catcolab--request "get_doc" ref-id))

(defun causal-catcolab-head-snapshot (ref-id)
  "Get the latest JSON snapshot of the document at REF-ID."
  (causal-catcolab--request "head_snapshot" ref-id))

(defun causal-catcolab-new-ref (content)
  "Create a new CatColab ref with CONTENT (an alist).
Returns the new ref UUID string."
  (causal-catcolab--request "new_ref" content))

(defun causal-catcolab-children (ref-id)
  "Get child ref stubs of REF-ID."
  (causal-catcolab--request "get_ref_children_stubs" ref-id))

(defun causal-catcolab-create-snapshot (ref-id)
  "Persist a snapshot of REF-ID to the database."
  (causal-catcolab--request "create_snapshot" ref-id))

;;; Model Content Constructors

;; CatColab document types (from catlog stdlib):
;;   simple-olog, simple-schema, causal-loop, causal-loop-delays,
;;   indeterminate-causal-loop, petri-net, reg-net,
;;   primitive-stock-flow, primitive-signed-stock-flow,
;;   power-system, unary-dec, empty

(defun causal-catcolab--new-model (theory-id name)
  "Construct a new CatColab model document alist.
THEORY-ID is the catlog theory identifier (e.g. \"simple-olog\").
NAME is the human-readable model name."
  `(("type" . "model")
    ("name" . ,name)
    ("theory" . ,theory-id)
    ("notebook" . (("tag" . "Notebook") ("cells" . [])))))

(defun causal-catcolab--olog-from-proof (name hypotheses goal)
  "Build an olog document from a proof state.
NAME is the model name. HYPOTHESES is a list of (name . type) pairs.
GOAL is the proof goal string.

Maps: hypotheses → olog Types, goal → distinguished Type,
implications between hyps → Aspects (morphisms)."
  (let* ((hyp-obs (mapcar (lambda (h)
                            `(("tag" . "ObType")
                              ("obType" . (("tag" . "Basic") ("content" . "Object")))
                              ("name" . ,(car h))
                              ("description" . ,(cdr h))
                              ("id" . ,(format "hyp-%s" (car h)))))
                          hypotheses))
         (goal-ob `(("tag" . "ObType")
                    ("obType" . (("tag" . "Basic") ("content" . "Object")))
                    ("name" . "Goal")
                    ("description" . ,goal)
                    ("id" . "goal")))
         (objects (vconcat hyp-obs (list goal-ob))))
    `(("type" . "model")
      ("name" . ,name)
      ("theory" . "simple-olog")
      ("notebook" . (("tag" . "Notebook") ("cells" . [])))
      ("content" . (("ob_generators" . ,objects)
                    ("mor_generators" . []))))))

;;; Display Helpers

(defun causal-catcolab--stub-line (stub)
  "Format a RefStub STUB as a single display line."
  (let ((name (let ((n (alist-get 'name stub "")))
                (if (string-empty-p n) "(unnamed)" n)))
        (type (alist-get 'typeName stub "?"))
        (ref-id (alist-get 'refId stub ""))
        (owner (let ((o (alist-get 'owner stub)))
                 (if o (alist-get 'username o "(anon)") "public")))
        (created (let ((c (alist-get 'createdAt stub "")))
                   (substring c 0 (min 10 (length c))))))
    (format "  [%s] %-30s %-10s  %s  %s"
            type name ref-id owner created)))

(defun causal-catcolab-url-for-ref (ref-id)
  "Return the browser URL for REF-ID on catcolab.org."
  (format "https://catcolab.org/app/new/%s" ref-id))

(provide 'causal-catcolab-utils)
;;; causal-catcolab-utils.el ends here
