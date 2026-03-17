(ns causal.web.inertia
  "Inertia.js adapter for Pedestal interceptors.

  Inertia is a server-driven SPA protocol:
    First visit  → full HTML with page props in data-page attribute
    XHR (X-Inertia: true) → JSON page object
    Version mismatch → 409 Conflict, client does full reload

  PEDESTAL INTEGRATION:
    The `inertia` interceptor runs on :enter to:
      1. Detect X-Inertia header → mark request as Inertia-aware
      2. Check X-Inertia-Version against server asset version
      3. On version mismatch for GET requests → 409 (forces full reload)
      4. Attach ::render fn to context for downstream handlers

    The `inertia` interceptor runs on :leave to:
      1. If response has ::inertia-page metadata, wrap it properly

  Handlers use (inertia/render ctx component props) which returns
  the context with :response set.

  PROTOCOL (https://inertiajs.com/the-protocol):
    Request:  X-Inertia: true, X-Inertia-Version: <hash>
    Response: X-Inertia: true, {component, props, url, version}

  CROSS-REFERENCES:
    causal.web.routes   — route definitions using this adapter
    causal.web.backends — backend registry providing props data"
  (:require [clojure.data.json :as json]))

(def ^:dynamic *asset-version* "1")

(defn inertia-request?
  "True if the request carries the X-Inertia header."
  [request]
  (= "true" (get-in request [:headers "x-inertia"])))

(defn- page-object
  "Build the canonical Inertia page object."
  [request component props]
  {:component component
   :props     props
   :url       (:uri request)
   :version   *asset-version*})

(defn- html-page
  "Full HTML document embedding the Inertia page object in #app data-page."
  [page-json]
  (str "<!DOCTYPE html>\n"
       "<html lang=\"en\">\n"
       "<head>\n"
       "  <meta charset=\"utf-8\">\n"
       "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
       "  <title>Causal Proof</title>\n"
       "</head>\n"
       "<body>\n"
       "  <div id=\"app\" data-page='" page-json "'></div>\n"
       "  <script src=\"/app.js\"></script>\n"
       "</body>\n"
       "</html>"))

(defn inertia-response
  "Build an Inertia.js response for COMPONENT + PROPS.
  Partial JSON for Inertia XHR, full HTML for first visit."
  [request component props]
  (let [po   (page-object request component props)
        json (json/write-str po)]
    (if (inertia-request? request)
      {:status  200
       :headers {"Content-Type" "application/json"
                 "X-Inertia"    "true"
                 "Vary"         "Accept"}
       :body    json}
      {:status  200
       :headers {"Content-Type" "text/html; charset=utf-8"}
       :body    (html-page json)})))

(defn render
  "Set the Inertia response on the Pedestal context.
  Use this inside interceptor :enter or :leave fns:
    (inertia/render ctx \"Backends\" {:backends data})"
  [ctx component props]
  (assoc ctx :response
         (inertia-response (:request ctx) component props)))

(def inertia
  "Pedestal interceptor implementing the Inertia.js protocol.

  :enter
    - Detects Inertia requests, attaches ::inertia? flag
    - Version conflict → 409 with X-Inertia-Location (GET only)
    - Attaches ::render fn for convenience

  :leave
    - Adds Vary: Accept header to all Inertia responses
    - Converts 302 redirects to 303 for PUT/PATCH/DELETE (Inertia spec)"
  {:name  ::inertia
   :enter (fn [ctx]
            (let [request (:request ctx)
                  is-inertia (inertia-request? request)
                  client-version (get-in request [:headers "x-inertia-version"])]
              (if (and is-inertia
                       client-version
                       (not= client-version *asset-version*)
                       (= :get (:request-method request)))
                ;; Version mismatch on GET → 409, client does full reload
                (assoc ctx :response
                       {:status  409
                        :headers {"X-Inertia-Location" (:uri request)}
                        :body    ""})
                ;; Normal flow: mark and continue
                (assoc ctx
                       ::inertia? is-inertia
                       ::render (fn [component props]
                                  (inertia-response request component props))))))

   :leave (fn [ctx]
            (let [resp (:response ctx)
                  request (:request ctx)
                  method (:request-method request)]
              (cond-> ctx
                ;; Inertia spec: PUT/PATCH/DELETE redirects must be 303, not 302
                (and (::inertia? ctx)
                     (#{:put :patch :delete} method)
                     (= 302 (:status resp)))
                (assoc-in [:response :status] 303)

                ;; Add Vary header for cacheability
                (::inertia? ctx)
                (assoc-in [:response :headers "Vary"] "Accept"))))})
