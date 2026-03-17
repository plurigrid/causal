(ns causal.web.backends
  "Causal-proof backend registry — Clojure mirror of causal-proof-utils.el.

  ARCHITECTURE (for Amp and other agents reading this file):

    causal-proof-utils.el  — Emacs backend registry (plist dispatch)
    backends.clj           — YOU ARE HERE: HTTP-accessible mirror
    inertia.clj            — Inertia.js adapter for server-driven props
    routes.clj             — Pedestal routes wiring everything together

  Each backend map has:
    :name     String — human-readable name
    :detect   Fn     — (fn [ctx]) returns truthy when applicable
    :ops      Set    — supported operations as keywords
    :mode     String — Emacs major-mode name for detection
    :ext      String — file extension (optional)")

(def backends
  "Ordered list of proof backends mirroring `causal-proof-backends` in Emacs.
  Each is a map with :name, :mode, :ext, :ops."
  [{:name "Narya"
    :mode "narya-mode"
    :ops  #{:step-fwd :step-back :to-point :process :retract
            :locked-end :goals :context :search :layout}}

   {:name "Proof General"
    :mode "proof-mode"
    :ops  #{:step-fwd :step-back :to-point :process :retract
            :locked-end :goals :context :search :layout}}

   {:name "Lean"
    :mode "lean4-mode"
    :ops  #{:step-fwd :step-back :to-point :process :retract
            :locked-end :goals :context :search}}

   {:name "OCaml"
    :mode "tuareg-mode"
    :ops  #{:step-fwd :step-back :to-point :process
            :goals :context :search :typecheck :compile}}

   {:name "DoubleTT"
    :mode "doublett-mode"
    :ext  ".dtt"
    :ops  #{:step-fwd :step-back :to-point :process
            :goals :context :search}}])

(defn detect-backend
  "Given a filename or mode hint, return the first matching backend or nil."
  [{:keys [filename mode]}]
  (or (when mode
        (first (filter #(= (:mode %) mode) backends)))
      (when filename
        (first (filter (fn [b]
                         (and (:ext b)
                              (.endsWith ^String filename (:ext b))))
                       backends)))
      nil))

(defn backend-summary
  "Return a summary of all backends for display."
  []
  (mapv (fn [b]
          {:name (:name b)
           :mode (:mode b)
           :ext  (:ext b "")
           :ops  (mapv name (sort (:ops b)))})
        backends))

(defn backend-by-name
  "Find backend by exact name."
  [n]
  (first (filter #(= (:name %) n) backends)))
