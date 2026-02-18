;;; sophia-mnemosyne.el --- Mnemosyne RDF knowledge graph from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, knowledge

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

;; Emacs interface to sophia-labs/mnemosyne-mcp (neem).
;; Provides commands to interact with the RDF knowledge graph
;; for storing proof transcripts, creating semantic wires,
;; and querying via SPARQL.
;;
;; The same backend is shared by Claude Code, Codex, Gemini,
;; Copilot, Droid, Vibe, and Kimi CLIs.
;;
;; INSTALLATION
;; (require 'sophia-mnemosyne)
;;
;; SETUP
;; 1. Install neem: cd ~/i/mnemosyne-mcp && uv tool install -e .
;; 2. Start backend: neem serve
;; 3. Configure CLIs: bash ~/i/causal/.topos/scripts/setup-mnemosyne-all-clis.sh

;;; Code:

(require 'json)
(require 'url)

;;; Customization

(defgroup sophia-mnemosyne nil
  "Mnemosyne knowledge graph integration."
  :group 'tools
  :prefix "sophia-mnemosyne-")

(defcustom sophia-mnemosyne-url "http://127.0.0.1:8001"
  "Mnemosyne FastAPI backend URL."
  :type 'string
  :group 'sophia-mnemosyne)

(defcustom sophia-mnemosyne-dev-token "dev-local"
  "Development token for local auth bypass."
  :type 'string
  :group 'sophia-mnemosyne)

;;; API Layer

(defun sophia-mnemosyne--api (method endpoint &optional body)
  "Call Mnemosyne API with METHOD on ENDPOINT, optional BODY.
Returns parsed JSON response."
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " sophia-mnemosyne-dev-token))))
         (url-request-data (when body (encode-coding-string
                                       (json-encode body) 'utf-8)))
         (full-url (concat sophia-mnemosyne-url endpoint)))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously full-url t nil 10)
          (goto-char (1+ url-http-end-of-headers))
          (json-read))
      (error
       (message "Mnemosyne API error: %s" (error-message-string err))
       nil))))

;;; Graph Management

(defun sophia-mnemosyne-list-graphs ()
  "List all knowledge graphs in a buffer."
  (interactive)
  (let ((graphs (sophia-mnemosyne--api "GET" "/graphs")))
    (if graphs
        (with-current-buffer (get-buffer-create "*Mnemosyne Graphs*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "Mnemosyne Knowledge Graphs\n"
                                'face 'bold))
            (insert (make-string 40 ?─) "\n\n")
            (if (vectorp graphs)
                (mapc (lambda (g)
                        (insert (format "  %s\n"
                                        (or (alist-get 'name g)
                                            (alist-get 'id g)
                                            g))))
                      graphs)
              (insert (format "  %s\n" graphs)))
            (special-mode))
          (display-buffer (current-buffer)))
      (message "No graphs or backend unreachable."))))

(defun sophia-mnemosyne-create-graph (name)
  "Create a new knowledge graph named NAME."
  (interactive "sGraph name: ")
  (let ((result (sophia-mnemosyne--api
                 "POST" "/graphs"
                 `((name . ,name)))))
    (if result
        (message "Graph '%s' created." name)
      (message "Failed to create graph."))))

;;; SPARQL

(defun sophia-mnemosyne-sparql-query (query)
  "Execute SPARQL QUERY against current graph."
  (interactive "sSPARQL: ")
  (let ((result (sophia-mnemosyne--api
                 "POST" "/sparql"
                 `((query . ,query)))))
    (when result
      (with-current-buffer (get-buffer-create "*SPARQL Results*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "SPARQL Results\n" 'face 'bold))
          (insert (make-string 40 ?─) "\n\n")
          (insert (pp-to-string result))
          (special-mode))
        (display-buffer (current-buffer))))))

;;; Document / Proof Transcript Operations

(defun sophia-mnemosyne-write-document (name content)
  "Write document NAME with CONTENT to current graph."
  (interactive "sDocument name: \nsContent: ")
  (let ((result (sophia-mnemosyne--api
                 "POST" "/documents"
                 `((name . ,name) (content . ,content)))))
    (if result
        (message "Document '%s' saved." name)
      (message "Failed to save document."))))

(defun sophia-mnemosyne-save-buffer ()
  "Save current buffer as a Mnemosyne document."
  (interactive)
  (let ((name (file-name-nondirectory
               (or (buffer-file-name) (buffer-name))))
        (content (buffer-string)))
    (sophia-mnemosyne-write-document name content)))

(defun sophia-mnemosyne-read-document (name)
  "Read document NAME from current graph."
  (interactive "sDocument name: ")
  (let ((result (sophia-mnemosyne--api
                 "GET" (concat "/documents/" (url-hexify-string name)))))
    (when result
      (with-current-buffer (get-buffer-create
                            (format "*Mnemosyne: %s*" name))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (or (alist-get 'content result)
                      (pp-to-string result))))
        (display-buffer (current-buffer))))))

;;; Semantic Wires

(defun sophia-mnemosyne-create-wire (from-id to-id predicate)
  "Create semantic wire from FROM-ID to TO-ID with PREDICATE."
  (interactive "sFrom ID: \nsTo ID: \nsPredicate: ")
  (let ((result (sophia-mnemosyne--api
                 "POST" "/wires"
                 `((from . ,from-id)
                   (to . ,to-id)
                   (predicate . ,predicate)))))
    (if result
        (message "Wire: %s -[%s]-> %s" from-id predicate to-id)
      (message "Failed to create wire."))))

(defun sophia-mnemosyne-traverse-wires (node-id)
  "Traverse wires from NODE-ID."
  (interactive "sNode ID: ")
  (let ((result (sophia-mnemosyne--api
                 "GET" (concat "/wires/" (url-hexify-string node-id)))))
    (when result
      (with-current-buffer (get-buffer-create "*Mnemosyne Wires*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "Wires from %s\n" node-id)
                              'face 'bold))
          (insert (make-string 40 ?─) "\n\n")
          (insert (pp-to-string result))
          (special-mode))
        (display-buffer (current-buffer))))))

;;; Status

(defun sophia-mnemosyne-status ()
  "Check Mnemosyne backend status."
  (interactive)
  (condition-case _
      (let ((result (sophia-mnemosyne--api "GET" "/health")))
        (message "Mnemosyne: %s at %s"
                 (if result "connected" "no response")
                 sophia-mnemosyne-url))
    (error (message "Mnemosyne: unreachable at %s" sophia-mnemosyne-url))))

(provide 'sophia-mnemosyne)
;;; sophia-mnemosyne.el ends here
