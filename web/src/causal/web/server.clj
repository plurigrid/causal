(ns causal.web.server
  "Pedestal HTTP server for causal-proof backend status + dispatch.

  Start:  clj -M:run
  REPL:   clj -M:dev  then (start) / (stop) / (restart)

  Serves:
    /                  — Inertia.js dashboard (backends list)
    /api/backends      — JSON backend registry
    /api/dispatch      — POST operation dispatch (returns emacsclient cmd)
    /api/health        — Health check

  CROSS-REFERENCES:
    causal-proof-utils.el   — Emacs backend registry (source of truth)
    causal.web.backends     — Clojure mirror of that registry
    causal.web.inertia      — Inertia.js protocol adapter
    causal.web.routes       — Route definitions"
  (:require [causal.web.routes :as routes]
            [io.pedestal.http :as http])
  (:gen-class))

(def service-map
  {::http/routes routes/routes
   ::http/type   :jetty
   ::http/port   8420
   ::http/host   "0.0.0.0"
   ;; Allow CORS for local dev (Inertia frontend may run on different port)
   ::http/allowed-origins {:creds true :allowed-origins (constantly true)}
   ;; Serve static files from resources/public
   ::http/resource-path "/public"})

(defonce server (atom nil))

(defn start []
  (reset! server (-> service-map
                     http/default-interceptors
                     http/create-server
                     http/start)))

(defn stop []
  (when @server
    (http/stop @server)
    (reset! server nil)))

(defn restart []
  (stop)
  (start))

(defn -main [& _args]
  (println "Starting causal-proof-web on http://localhost:8420")
  (println "  GET  /api/backends      — list backends")
  (println "  GET  /api/backends/:name — backend detail")
  (println "  POST /api/dispatch      — dispatch op")
  (println "  GET  /api/health        — health check")
  (println "  GET  /                  — Inertia.js dashboard")
  (start))
