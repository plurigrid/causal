#!/usr/bin/env bb
;; setup-mnemosyne-all-clis.bb
;; Configure mnemosyne-mcp for all local AI CLIs
;; Source: sophia-labs/mnemosyne-mcp

(ns setup-mnemosyne
  (:require [babashka.fs :as fs]
            [babashka.process :refer [shell process]]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def api-url  (or (System/getenv "MNEMOSYNE_FASTAPI_URL") "http://127.0.0.1:8001"))
(def dev-token (or (System/getenv "MNEMOSYNE_DEV_TOKEN") "dev-local"))
(def dev-user  (or (System/getenv "MNEMOSYNE_DEV_USER_ID") "bmorphism"))
(def home      (System/getProperty "user.home"))

(defn home-path [& segments]
  (str (apply fs/path home segments)))

;; --- JSON configs ---

(def mcp-json
  {:mcpServers
   {:mnemosyne
    {:command "uv"
     :args ["run" "neem-mcp-server"]
     :env {"MNEMOSYNE_FASTAPI_URL" api-url
           "MNEMOSYNE_DEV_TOKEN"   dev-token
           "MNEMOSYNE_DEV_USER_ID" dev-user
           "LOG_LEVEL"             "ERROR"}}}})

(def copilot-json
  {:servers
   {:mnemosyne
    {:command "uv"
     :args ["run" "neem-mcp-server"]
     :env {"MNEMOSYNE_FASTAPI_URL" api-url
           "LOG_LEVEL"             "ERROR"}}}})

(def vibe-edn
  (pr-str
   {:mcp-servers
    {:mnemosyne
     {:command "uv"
      :args ["run" "neem-mcp-server"]
      :env {"MNEMOSYNE_FASTAPI_URL" api-url
            "MNEMOSYNE_DEV_TOKEN"   dev-token
            "MNEMOSYNE_DEV_USER_ID" dev-user}}}}))

;; --- Helpers ---

(defn cmd-exists? [cmd]
  (try
    (-> (process {:out :string :err :string} "which" cmd)
        deref :exit (= 0))
    (catch Exception _ false)))

(defn write-json! [dir filename data]
  (let [d (home-path dir)]
    (fs/create-dirs d)
    (spit (str (fs/path d filename)) (json/generate-string data))
    true))

(defn status [sym label]
  (println (str "  " sym " " label)))

;; --- CLI configurators ---

(defn setup-neem! []
  (when-not (cmd-exists? "neem")
    (let [mcp-dir (home-path "i" "mnemosyne-mcp")]
      (if (fs/exists? mcp-dir)
        (do (println "Installing neem...")
            (shell {:dir mcp-dir} "uv" "tool" "install" "-e" "."))
        (do (println "  Clone first:")
            (println "    git clone https://github.com/sophia-labs/mnemosyne-mcp.git ~/i/mnemosyne-mcp")
            (println "  Then re-run this script."))))))

(defn setup-claude! []
  (if (cmd-exists? "claude")
    (do (try (shell "claude" "mcp" "add" "mnemosyne" "--" "uv" "run" "neem-mcp-server")
             (catch Exception _ nil))
        (status "✓" "Claude Code"))
    (status "○" "Claude Code (not found, skipping)")))

(defn setup-codex! []
  (write-json! ".codex" "mcp.json" mcp-json)
  (status "✓" "Codex"))

(defn setup-gemini! []
  (write-json! ".gemini" "settings.json" mcp-json)
  (status "✓" "Gemini CLI"))

(defn setup-copilot! []
  (write-json! ".config/github-copilot" "mcp.json" copilot-json)
  (status "✓" "Copilot"))

(defn setup-droid! []
  (write-json! ".droid" "mcp.json" mcp-json)
  (status "✓" "Droid"))

(defn setup-vibe! []
  (let [d (home-path "i" "vibe")]
    (fs/create-dirs d)
    (spit (str (fs/path d "mcp-mnemosyne.edn")) vibe-edn)
    (status "✓" "Vibe")))

(defn setup-kimi! []
  (write-json! ".kimi" "mcp.json" mcp-json)
  (status "✓" "Kimi"))

;; --- Main ---

(defn -main []
  (println "=== Mnemosyne MCP — Multi-CLI Setup ===")
  (println (str "API: " api-url))
  (println)

  (setup-neem!)
  (setup-claude!)
  (setup-codex!)
  (setup-gemini!)
  (setup-copilot!)
  (setup-droid!)
  (setup-vibe!)
  (setup-kimi!)

  (println)
  (println (str "=== Done. " api-url " configured for 7 CLIs ==="))
  (println)
  (println "Next steps:")
  (println "  1. Start backend: neem serve (or kubectl port-forward)")
  (println "  2. Test: neem init")
  (println "  3. Use from any CLI: list_graphs, sparql_query, write_document"))

(-main)
