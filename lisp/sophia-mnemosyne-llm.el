;;; sophia-mnemosyne-llm.el --- Self-operating LLM bridge for Mnemosyne -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Plurigrid Contributors

;; Author: bmorphism
;; Keywords: tools, knowledge, ai

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Self-operating LLM bridge connecting:
;;   gptel (Emacs LLM client) + Ollama (local, free)
;;   ↕ mcp.el (MCP client in Emacs)
;;   ↕ neem-mcp-server (Mnemosyne knowledge graph)
;;   ↕ emacs-mcp-server (Emacs as tool target for external agents)
;;
;; Architecture (SDF Ch.7 propagator network):
;;
;;   ┌──────────┐    ┌──────────┐    ┌──────────────┐
;;   │  Ollama  │◄──►│  gptel   │◄──►│  mcp.el      │
;;   │  (local) │    │  (Emacs) │    │  (MCP client) │
;;   └──────────┘    └──────────┘    └──────┬───────┘
;;                                          │
;;                                   ┌──────▼───────┐
;;                                   │ neem-mcp-srv │
;;                                   │ (knowledge)  │
;;                                   └──────────────┘
;;
;; No API keys. No cloud. Fully local. Self-operating.
;;
;; Requires: gptel, sophia-mnemosyne
;; Optional: mcp (mcp.el from lizqwerscott), ellama

;;; Code:

(require 'sophia-mnemosyne)

;;; ─── Customization ─────────────────────────────────────────────────

(defgroup sophia-mnemosyne-llm nil
  "LLM integration for Mnemosyne knowledge graph."
  :group 'sophia-mnemosyne
  :prefix "sophia-mnemosyne-llm-")

(defcustom sophia-mnemosyne-llm-model "qwen3:4b"
  "Ollama model for self-operating queries.
Must support tool-use for full autonomous operation.
Good free options: qwen3:4b, llama3.2:3b, mistral:7b."
  :type 'string
  :group 'sophia-mnemosyne-llm)

(defcustom sophia-mnemosyne-llm-ollama-host "http://localhost:11434"
  "Ollama API endpoint."
  :type 'string
  :group 'sophia-mnemosyne-llm)

(defcustom sophia-mnemosyne-llm-auto-save-transcripts t
  "When non-nil, automatically save gptel conversations to Mnemosyne."
  :type 'boolean
  :group 'sophia-mnemosyne-llm)

;;; ─── gptel Configuration ───────────────────────────────────────────

(defun sophia-mnemosyne-llm-setup-gptel ()
  "Configure gptel to use Ollama with Mnemosyne context.
Registers a `mnemosyne' gptel backend pointing at local Ollama."
  (interactive)
  (require 'gptel)
  ;; Register Ollama backend if not already present
  (unless (alist-get "mnemosyne-ollama" gptel--known-backends nil nil #'string=)
    (gptel-make-ollama "mnemosyne-ollama"
      :host (replace-regexp-in-string "^https?://" ""
                                      sophia-mnemosyne-llm-ollama-host)
      :stream t
      :models (list sophia-mnemosyne-llm-model)))
  (message "gptel: mnemosyne-ollama backend registered (%s)"
           sophia-mnemosyne-llm-model))

;;; ─── Tool Definitions for gptel ────────────────────────────────────

;; These functions are exposed as tools that gptel can call via tool-use.
;; SDF Ch.9 generic procedures: dispatch on what the LLM needs.

(defvar sophia-mnemosyne-llm--tools nil
  "Alist of Mnemosyne tools for gptel tool-use.")

(defun sophia-mnemosyne-llm--define-tools ()
  "Define Mnemosyne operations as gptel-compatible tools."
  (require 'gptel)
  (when (fboundp 'gptel-make-tool)
    (setq sophia-mnemosyne-llm--tools
          (list
           (gptel-make-tool
            :function (lambda (query)
                        (let ((result (sophia-mnemosyne--api
                                       "POST" "/sparql"
                                       `((query . ,query)))))
                          (if result (pp-to-string result)
                            "No results or backend unreachable.")))
            :name "sparql_query"
            :description "Execute a SPARQL query against the Mnemosyne knowledge graph. Returns RDF triples matching the query."
            :args (list '(:name "query"
                         :type "string"
                         :description "SPARQL query string (e.g., SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10)")))

           (gptel-make-tool
            :function (lambda ()
                        (let ((result (sophia-mnemosyne--api "GET" "/graphs")))
                          (if result (pp-to-string result)
                            "No graphs found or backend unreachable.")))
            :name "list_graphs"
            :description "List all knowledge graphs available in Mnemosyne."
            :args nil)

           (gptel-make-tool
            :function (lambda (name content)
                        (let ((result (sophia-mnemosyne--api
                                       "POST" "/documents"
                                       `((name . ,name) (content . ,content)))))
                          (if result (format "Document '%s' saved." name)
                            (format "Failed to save document '%s'." name))))
            :name "write_document"
            :description "Write a document (proof transcript, note, or any text) to the current Mnemosyne graph."
            :args (list '(:name "name"
                         :type "string"
                         :description "Document name/identifier")
                        '(:name "content"
                         :type "string"
                         :description "Document content text")))

           (gptel-make-tool
            :function (lambda (name)
                        (let ((result (sophia-mnemosyne--api
                                       "GET" (concat "/documents/"
                                                     (url-hexify-string name)))))
                          (if result
                              (or (alist-get 'content result)
                                  (pp-to-string result))
                            (format "Document '%s' not found." name))))
            :name "read_document"
            :description "Read a document from the Mnemosyne knowledge graph."
            :args (list '(:name "name"
                         :type "string"
                         :description "Document name to retrieve")))

           (gptel-make-tool
            :function (lambda (from-id to-id predicate)
                        (let ((result (sophia-mnemosyne--api
                                       "POST" "/wires"
                                       `((from . ,from-id)
                                         (to . ,to-id)
                                         (predicate . ,predicate)))))
                          (if result
                              (format "Wire created: %s -[%s]-> %s" from-id predicate to-id)
                            "Failed to create wire.")))
            :name "create_wire"
            :description "Create a semantic wire (RDF triple) linking two nodes with a predicate. Predicates use Kantian categories: partOf, contains, supports, contradicts, causeOf, requires, enables, precedes, flowsInto, divergesFrom."
            :args (list '(:name "from_id"
                         :type "string"
                         :description "Source node ID")
                        '(:name "to_id"
                         :type "string"
                         :description "Target node ID")
                        '(:name "predicate"
                         :type "string"
                         :description "Relationship predicate (e.g., supports, causeOf, requires)")))

           (gptel-make-tool
            :function (lambda (node-id)
                        (let ((result (sophia-mnemosyne--api
                                       "GET" (concat "/wires/"
                                                     (url-hexify-string node-id)))))
                          (if result (pp-to-string result)
                            (format "No wires from '%s'." node-id))))
            :name "traverse_wires"
            :description "Traverse all semantic wires from a given node, returning connected nodes and predicates."
            :args (list '(:name "node_id"
                         :type "string"
                         :description "Node ID to traverse from")))))))

;;; ─── MCP Client Bridge ─────────────────────────────────────────────

(defun sophia-mnemosyne-llm-setup-mcp ()
  "Connect mcp.el to neem-mcp-server if mcp.el is available.
This lets gptel access Mnemosyne tools through the MCP protocol."
  (interactive)
  (if (require 'mcp nil t)
      (progn
        (unless (assoc "mnemosyne" mcp-server-connections)
          (mcp-add-server "mnemosyne"
                          :command "uv"
                          :args '("run" "neem-mcp-server")
                          :env `(("MNEMOSYNE_FASTAPI_URL" . ,sophia-mnemosyne-url)
                                 ("MNEMOSYNE_DEV_TOKEN" . ,sophia-mnemosyne-dev-token)
                                 ("LOG_LEVEL" . "ERROR"))))
        (message "mcp.el: mnemosyne server registered"))
    (message "mcp.el not installed; using direct API tools instead")))

;;; ─── Self-Operating Commands ───────────────────────────────────────

(defun sophia-mnemosyne-llm-ask (prompt)
  "Ask the local LLM about the knowledge graph with PROMPT.
The LLM has tool-use access to query, write, and wire Mnemosyne."
  (interactive "sAsk Mnemosyne: ")
  (require 'gptel)
  (sophia-mnemosyne-llm-setup-gptel)
  (sophia-mnemosyne-llm--define-tools)
  (let* ((backend (alist-get "mnemosyne-ollama" gptel--known-backends
                             nil nil #'string=))
         (gptel-backend backend)
         (gptel-model sophia-mnemosyne-llm-model)
         (gptel-tools sophia-mnemosyne-llm--tools))
    (gptel-request prompt
      :system "You are a knowledge graph assistant. You have access to a Mnemosyne RDF knowledge graph containing proof transcripts, semantic wires, and documents. Use the provided tools to answer questions by querying the graph. Always cite specific documents or wires when possible."
      :callback (lambda (response info)
                  (if response
                      (with-current-buffer (get-buffer-create "*Mnemosyne LLM*")
                        (let ((inhibit-read-only t))
                          (goto-char (point-max))
                          (insert (propertize (format "\n── %s ──\n" prompt)
                                              'face 'bold))
                          (insert response "\n"))
                        (special-mode)
                        (display-buffer (current-buffer)))
                    (message "LLM error: %s" (plist-get info :error)))))))

(defun sophia-mnemosyne-llm-summarize-buffer ()
  "Send current buffer to LLM, save summary as Mnemosyne document."
  (interactive)
  (require 'gptel)
  (sophia-mnemosyne-llm-setup-gptel)
  (let* ((buf-name (file-name-nondirectory
                    (or (buffer-file-name) (buffer-name))))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (prompt (format "Summarize this document concisely:\n\n%s" content))
         (backend (alist-get "mnemosyne-ollama" gptel--known-backends
                             nil nil #'string=))
         (gptel-backend backend)
         (gptel-model sophia-mnemosyne-llm-model))
    (gptel-request prompt
      :system "You are a technical summarizer. Produce a concise summary preserving key theorems, definitions, and proof structure."
      :callback (lambda (response info)
                  (if response
                      (progn
                        (sophia-mnemosyne-write-document
                         (concat "summary:" buf-name) response)
                        (message "Summary saved to Mnemosyne: summary:%s" buf-name))
                    (message "Summarization failed: %s"
                             (plist-get info :error)))))))

(defun sophia-mnemosyne-llm-auto-wire-buffer ()
  "Analyze current buffer with LLM and create semantic wires automatically."
  (interactive)
  (require 'gptel)
  (sophia-mnemosyne-llm-setup-gptel)
  (sophia-mnemosyne-llm--define-tools)
  (let* ((buf-name (file-name-nondirectory
                    (or (buffer-file-name) (buffer-name))))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (prompt (format "Analyze this document and create semantic wires to connect it to related concepts in the knowledge graph. First list_graphs and traverse existing wires, then create new wires using Kantian predicates (supports, causeOf, requires, enables, partOf). Document name: %s\n\nContent:\n%s"
                         buf-name content))
         (backend (alist-get "mnemosyne-ollama" gptel--known-backends
                             nil nil #'string=))
         (gptel-backend backend)
         (gptel-model sophia-mnemosyne-llm-model)
         (gptel-tools sophia-mnemosyne-llm--tools))
    (gptel-request prompt
      :system "You are a knowledge graph curator. Analyze documents and create semantic wires connecting them to the existing graph. Use the create_wire tool with appropriate Kantian predicates."
      :callback (lambda (response info)
                  (if response
                      (message "Auto-wiring complete for %s" buf-name)
                    (message "Auto-wiring failed: %s"
                             (plist-get info :error)))))))

;;; ─── gptel Conversation Persistence ────────────────────────────────

(defun sophia-mnemosyne-llm--save-conversation ()
  "Save the current gptel buffer to Mnemosyne as a transcript.
Intended as `gptel-post-response-functions' hook."
  (when (and sophia-mnemosyne-llm-auto-save-transcripts
             (bound-and-true-p gptel-mode))
    (let ((name (format "gptel:%s:%s"
                        (file-name-nondirectory
                         (or (buffer-file-name) (buffer-name)))
                        (format-time-string "%Y%m%dT%H%M%S")))
          (content (buffer-substring-no-properties (point-min) (point-max))))
      (sophia-mnemosyne--api "POST" "/documents"
                             `((name . ,name) (content . ,content))))))

;;; ─── Initialization ────────────────────────────────────────────────

(defun sophia-mnemosyne-llm-init ()
  "Initialize the self-operating Mnemosyne LLM bridge.
Call this from init.el or interactively."
  (interactive)
  ;; 1. gptel + Ollama
  (when (require 'gptel nil t)
    (sophia-mnemosyne-llm-setup-gptel)
    (sophia-mnemosyne-llm--define-tools)
    ;; Auto-save gptel conversations to Mnemosyne
    (when sophia-mnemosyne-llm-auto-save-transcripts
      (add-hook 'gptel-post-response-functions
                #'sophia-mnemosyne-llm--save-conversation)))
  ;; 2. mcp.el bridge (optional)
  (sophia-mnemosyne-llm-setup-mcp)
  (message "sophia-mnemosyne-llm: initialized (model=%s)"
           sophia-mnemosyne-llm-model))

(provide 'sophia-mnemosyne-llm)
;;; sophia-mnemosyne-llm.el ends here
