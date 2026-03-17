(ns causal.web.routes
  "Pedestal routes for causal-proof backend status and dispatch.

  All routes use Pedestal interceptor maps, not bare handler fns.
  The Inertia interceptor is wired into Inertia-serving routes via
  `with-inertia`, which prepends it to the interceptor chain.

  ROUTES:
    GET  /                  — Inertia dashboard (Backends page)
    GET  /api/backends      — JSON: all backends + ops
    GET  /api/backends/:name — JSON: single backend detail
    POST /api/dispatch      — JSON: dispatch op (returns emacsclient cmd)
    GET  /api/health        — JSON: health check

  INTERCEPTOR CHAIN:
    Inertia routes:  [inertia/inertia, handler]
    API routes:      [json-body, handler]

  CROSS-REFERENCES:
    causal.web.backends  — backend registry (mirrors causal-proof-utils.el)
    causal.web.inertia   — Inertia.js protocol interceptor"
  (:require [causal.web.backends :as backends]
            [causal.web.inertia :as inertia]
            [clojure.data.json :as json]
            [io.pedestal.http.route :as route]))

;;; ── Utility interceptors ─────────────────────────────────────────────

(defn json-response [status body]
  {:status  status
   :headers {"Content-Type" "application/json"}
   :body    (json/write-str body)})

(def json-body
  "Interceptor that parses JSON request body into :json-params."
  {:name  ::json-body
   :enter (fn [ctx]
            (let [body (some-> ctx :request :body slurp)]
              (if (and body (not (.isEmpty ^String body)))
                (assoc-in ctx [:request :json-params]
                          (json/read-str body :key-fn keyword))
                ctx)))})

;;; ── Inertia page interceptors ────────────────────────────────────────

(def index-page
  "GET / — Inertia dashboard showing all backends."
  {:name  ::index-page
   :enter (fn [ctx]
            (inertia/render ctx "Backends"
                           {:backends (backends/backend-summary)
                            :active   nil}))})

;;; ── API interceptors ─────────────────────────────────────────────────

(def list-backends
  "GET /api/backends — all registered backends."
  {:name  ::list-backends
   :enter (fn [ctx]
            (assoc ctx :response
                   (json-response 200 {:backends (backends/backend-summary)})))})

(def get-backend
  "GET /api/backends/:name — single backend by name."
  {:name  ::get-backend
   :enter (fn [ctx]
            (let [n (get-in ctx [:request :path-params :name])
                  b (backends/backend-by-name n)]
              (assoc ctx :response
                     (if b
                       (json-response 200
                                      {:backend {:name (:name b)
                                                 :mode (:mode b)
                                                 :ext  (:ext b "")
                                                 :ops  (mapv name (sort (:ops b)))}})
                       (json-response 404
                                      {:error (str "Backend not found: " n)})))))})

(def dispatch-op
  "POST /api/dispatch — dispatch an operation to a backend.
  Body: {\"backend\": \"Narya\", \"op\": \"step-fwd\"}
  Returns the emacsclient eval string for actual execution."
  {:name  ::dispatch-op
   :enter (fn [ctx]
            (let [body  (get-in ctx [:request :json-params])
                  bname (:backend body)
                  op    (:op body)
                  b     (backends/backend-by-name bname)]
              (assoc ctx :response
                     (cond
                       (nil? b)
                       (json-response 404 {:error (str "Unknown backend: " bname)})

                       (not (contains? (:ops b) (keyword op)))
                       (json-response 400 {:error (str "Operation " op
                                                       " not supported by " bname)})

                       :else
                       (json-response 200
                                      {:status      "dispatched"
                                       :backend     bname
                                       :op          op
                                       :emacsclient (str "(causal-proof--dispatch :" op ")")})))))})

(def health
  "GET /api/health"
  {:name  ::health
   :enter (fn [ctx]
            (assoc ctx :response
                   (json-response 200
                                  {:status   "ok"
                                   :service  "causal-proof-web"
                                   :version  inertia/*asset-version*
                                   :backends (count backends/backends)})))})

;;; ── Route table ──────────────────────────────────────────────────────

(def routes
  (route/expand-routes
   #{["/"                   :get  [inertia/inertia index-page]   :route-name :index]
     ["/api/backends"       :get  [list-backends]                :route-name :list-backends]
     ["/api/backends/:name" :get  [get-backend]                  :route-name :get-backend]
     ["/api/dispatch"       :post [json-body dispatch-op]        :route-name :dispatch]
     ["/api/health"         :get  [health]                       :route-name :health]}))
