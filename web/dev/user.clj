(ns user
  "REPL helpers for causal-proof-web development."
  (:require [causal.web.server :as server]))

(defn start [] (server/start))
(defn stop  [] (server/stop))
(defn restart [] (server/restart))

(comment
  (start)
  (stop)
  (restart)

  ;; Quick test:
  ;; curl http://localhost:8420/api/health
  ;; curl http://localhost:8420/api/backends
  ;; curl http://localhost:8420/api/backends/Narya
  ;; curl -X POST http://localhost:8420/api/dispatch \
  ;;   -H "Content-Type: application/json" \
  ;;   -d '{"backend":"OCaml","op":"typecheck"}'
  )
