#!/usr/bin/env bb
;; poincare-commerce.bb — Hyperbolic embeddings for commerce channel hierarchy
;;
;; Core idea (Nicol M. Kela / Nickel & Kiela 2017):
;;   Embed hierarchical data into the Poincaré disk where distances grow
;;   exponentially toward the boundary. A tree with branching factor b and
;;   depth d needs O(d) hyperbolic dimensions vs O(b^d) Euclidean dimensions.
;;
;; Commerce hierarchy:
;;   origin (0,0) = maximally capable (UCP live, direct MCP checkout)
;;   boundary |z|→1 = maximally constrained (manual, return-based, proxy)
;;
;; Depth encodes capability:  closer to origin = more automated
;; Angle encodes category:    angular separation = different channel class
;;
;; GF(3) trit sectors partition the disk into 3 wedges:
;;   PLUS  (+1) = [0°, 120°)    generation/ordering
;;   ERGODIC(0) = [120°, 240°)  coordination/routing
;;   MINUS (-1) = [240°, 360°)  validation/verification

(require '[clojure.string :as str])

;; ════════════════════════════════════════════════════════════════
;; Poincaré Disk Geometry
;;
;; The Poincaré disk model: D = {z ∈ ℝ² : |z| < 1}
;; Metric: ds² = 4(dx² + dy²) / (1 - |z|²)²
;;
;; Distance: d(u,v) = arcosh(1 + 2‖u-v‖² / ((1-‖u‖²)(1-‖v‖²)))
;;
;; Key property: volume grows exponentially toward boundary.
;; A circle of hyperbolic radius r has area 4π sinh²(r/2) ≈ πe^r.
;; Trees embed naturally: root at origin, leaves near boundary.
;; ════════════════════════════════════════════════════════════════

(defn norm-sq [[x y]]
  (+ (* x x) (* y y)))

(defn norm [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn vec-sub [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn vec-add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn vec-scale [s [x y]]
  [(* s x) (* s y)])

(defn clamp-to-disk
  "Project point into open disk |z| < 1-eps."
  [[x y] & [eps]]
  (let [eps (or eps 1e-6)
        r   (norm [x y])
        max-r (- 1.0 eps)]
    (if (>= r max-r)
      (vec-scale (/ max-r r) [x y])
      [x y])))

(defn poincare-distance
  "Hyperbolic distance in the Poincaré disk model.
   d(u,v) = arcosh(1 + 2‖u-v‖² / ((1-‖u‖²)(1-‖v‖²)))"
  [u v]
  (let [u (clamp-to-disk u)
        v (clamp-to-disk v)
        diff-sq (norm-sq (vec-sub u v))
        denom   (* (- 1.0 (norm-sq u))
                   (- 1.0 (norm-sq v)))
        arg     (+ 1.0 (/ (* 2.0 diff-sq) (max denom 1e-10)))]
    ;; arcosh(x) = ln(x + sqrt(x²-1))
    (Math/log (+ arg (Math/sqrt (max 0.0 (- (* arg arg) 1.0)))))))

;; ════════════════════════════════════════════════════════════════
;; Commerce Hierarchy → Poincaré Disk Embedding
;;
;; Radius encodes automation level (capability depth):
;;   r=0.0  UCP live (direct MCP checkout, tokenized payment)
;;   r=0.3  UCP pending (merchant exists, endpoint coming)
;;   r=0.5  Browser automation (Instacart, DoorDash via playwright)
;;   r=0.7  Concierge/stylist (human-mediated digital)
;;   r=0.85 Physical proxy (TaskRabbit, rent-a-human)
;;   r=0.95 Return-based (Zappos: order N, return N-1)
;;
;; Angle encodes GF(3) trit sector + channel class:
;;   PLUS sector [0°,120°):   generation channels
;;   ERGODIC sector [120°,240°): coordination channels
;;   MINUS sector [240°,360°): validation channels
;; ════════════════════════════════════════════════════════════════

(def TWO-PI (* 2.0 Math/PI))
(def SECTOR-WIDTH (/ TWO-PI 3.0))

(defn trit->sector-base [trit]
  (case trit
     1 0.0                          ;; PLUS: [0°, 120°)
     0 SECTOR-WIDTH                 ;; ERGODIC: [120°, 240°)
    -1 (* 2.0 SECTOR-WIDTH)))      ;; MINUS: [240°, 360°)

(defn polar->cartesian [r theta]
  [(* r (Math/cos theta))
   (* r (Math/sin theta))])

(def automation-radius
  {:ucp-live       0.05   ;; at the origin: maximum capability
   :ucp-pending    0.30
   :browser-auto   0.50
   :concierge      0.65
   :marketplace    0.45
   :same-day       0.55
   :aggregator     0.50
   :coop           0.55
   :p2p            0.70
   :courier        0.60
   :subscription   0.40
   :discount       0.65
   :thrift         0.75
   :resale         0.70
   :rent-a-human   0.85
   :luxury         0.60
   :mass-market    0.55
   :free-return    0.90
   :furniture      0.60
   :grocery        0.50
   :direct         0.35
   :artisan        0.80
   :specialist     0.40})

(def channels
  [{:letter "A" :name "amazon"         :trit  1 :class :marketplace  :ucp? false}
   {:letter "B" :name "allbirds-ucp"   :trit -1 :class :ucp-live     :ucp? true
    :mcp "allbirds.com/api/ucp/mcp"}
   {:letter "C" :name "cobbler-craft"  :trit  0 :class :artisan      :ucp? false}
   {:letter "D" :name "delivery-app"   :trit  1 :class :same-day     :ucp? false}
   {:letter "E" :name "ebay-resale"    :trit -1 :class :resale       :ucp? false}
   {:letter "F" :name "facebook-local" :trit  0 :class :p2p          :ucp? false}
   {:letter "G" :name "google-shop"    :trit  1 :class :aggregator   :ucp? false}
   {:letter "H" :name "human-expert"   :trit -1 :class :rent-a-human :ucp? false}
   {:letter "I" :name "ikea-deliver"   :trit  0 :class :furniture    :ucp? false}
   {:letter "J" :name "jet-courier"    :trit  1 :class :courier      :ucp? false}
   {:letter "K" :name "kit-subscribe"  :trit -1 :class :subscription :ucp? false}
   {:letter "L" :name "liquidation"    :trit  0 :class :discount     :ucp? false}
   {:letter "M" :name "mercari-p2p"    :trit  1 :class :resale       :ucp? false}
   {:letter "N" :name "nordstrom-styl" :trit -1 :class :concierge    :ucp? false}
   {:letter "O" :name "offerup-local"  :trit  0 :class :p2p          :ucp? false}
   {:letter "P" :name "prime-fresh"    :trit  1 :class :grocery      :ucp? false}
   {:letter "Q" :name "quick-errand"   :trit -1 :class :rent-a-human :ucp? false}
   {:letter "R" :name "rei-coop"       :trit  0 :class :coop         :ucp? false}
   {:letter "S" :name "specialty-web"  :trit  1 :class :specialist   :ucp? false}
   {:letter "T" :name "target-shipt"   :trit -1 :class :same-day     :ucp? false}
   {:letter "U" :name "used-thrift"    :trit  0 :class :thrift       :ucp? false}
   {:letter "V" :name "vendor-direct"  :trit  1 :class :direct       :ucp? false}
   {:letter "W" :name "walmart-plus"   :trit -1 :class :mass-market  :ucp? false}
   {:letter "X" :name "xpress-local"   :trit  0 :class :same-day     :ucp? false}
   {:letter "Y" :name "yoox-luxury"    :trit  1 :class :luxury       :ucp? false}
   {:letter "Z" :name "zappos-return"  :trit -1 :class :free-return  :ucp? false}])

(defn embed-channel
  "Embed a channel into the Poincaré disk.
   Radius = automation level, angle = GF(3) sector + offset within sector."
  [{:keys [letter trit class]} idx-in-sector total-in-sector]
  (let [r     (get automation-radius class 0.50)
        base  (trit->sector-base trit)
        ;; spread channels evenly within their 120° sector
        offset (* SECTOR-WIDTH (/ (+ idx-in-sector 0.5) (max total-in-sector 1)))
        theta (+ base offset)]
    (polar->cartesian r theta)))

(defn embed-all-channels
  "Embed all 26 channels into the Poincaré disk."
  []
  (let [by-trit (group-by :trit channels)]
    (mapv (fn [ch]
            (let [siblings (get by-trit (:trit ch))
                  idx      (.indexOf siblings ch)
                  total    (count siblings)
                  [x y]    (embed-channel ch idx total)]
              (assoc ch :x x :y y :r (norm [x y]))))
          channels)))

;; ════════════════════════════════════════════════════════════════
;; Hyperbolic Routing: find nearest channel to origin for a query
;;
;; Unlike Euclidean nearest-neighbor, hyperbolic distance from origin
;; is: d(0, z) = 2 * arctanh(|z|)
;; This grows slowly near origin, explosively near boundary.
;; A channel at r=0.05 (Allbirds UCP) is hyperbolically ~0.1 from origin.
;; A channel at r=0.95 (Zappos return) is hyperbolically ~3.66 from origin.
;; That's a 36x ratio vs 19x in Euclidean — hierarchy is amplified.
;; ════════════════════════════════════════════════════════════════

(defn hyperbolic-distance-from-origin
  "d(0, z) = 2 * arctanh(|z|) = ln((1+|z|)/(1-|z|))"
  [[x y]]
  (let [r (norm [x y])
        r (min r 0.9999)]
    (Math/log (/ (+ 1.0 r) (- 1.0 r)))))

(defn route-hyperbolic
  "Route an item to the nearest channel in hyperbolic space.
   Prefers channels closer to origin (more automated/capable)."
  [embedded-channels valid-letters]
  (let [valid (filter #(valid-letters (:letter %)) embedded-channels)]
    (sort-by #(hyperbolic-distance-from-origin [(:x %) (:y %)]) valid)))

;; ════════════════════════════════════════════════════════════════
;; CLI
;; ════════════════════════════════════════════════════════════════

(let [cmd (first *command-line-args*)
      embedded (embed-all-channels)]
  (case cmd

    "embed"
    (do
      (println "╔═══════════════════════════════════════════════════════════════════╗")
      (println "║  POINCARÉ DISK — Commerce Channel Embeddings                    ║")
      (println "║  Nicol M. Kela: hierarchical data in hyperbolic space           ║")
      (println "╚═══════════════════════════════════════════════════════════════════╝")
      (println)
      (println "  Origin (0,0) = maximum capability (UCP live, direct MCP checkout)")
      (println "  Boundary |z|→1 = minimum capability (manual, proxy, return-based)")
      (println "  GF(3) sectors: PLUS [0°,120°) | ERGODIC [120°,240°) | MINUS [240°,360°)")
      (println)
      (printf "  %-2s %-16s %-14s %6s %6s %8s %8s  %s%n"
              "" "Channel" "Class" "x" "y" "|z|" "d(0,z)" "UCP?")
      (printf "  %-2s %-16s %-14s %6s %6s %8s %8s  %s%n"
              "--" "----------------" "--------------" "------" "------" "--------" "--------" "----")
      (doseq [ch (sort-by :r embedded)]
        (let [d-origin (hyperbolic-distance-from-origin [(:x ch) (:y ch)])]
          (printf "  %-2s %-16s %-14s %6.3f %6.3f %8.4f %8.4f  %s%n"
                  (:letter ch) (:name ch) (name (:class ch))
                  (:x ch) (:y ch) (:r ch) d-origin
                  (if (:ucp? ch) "LIVE" ""))))
      (println)
      (println "  Hyperbolic amplification:")
      (let [allbirds (first (filter #(= "B" (:letter %)) embedded))
            zappos   (first (filter #(= "Z" (:letter %)) embedded))
            d-ab (hyperbolic-distance-from-origin [(:x allbirds) (:y allbirds)])
            d-zp (hyperbolic-distance-from-origin [(:x zappos) (:y zappos)])]
        (printf "    Allbirds (UCP live):  |z|=%.4f  d(0)=%.4f%n" (:r allbirds) d-ab)
        (printf "    Zappos (return-based): |z|=%.4f  d(0)=%.4f%n" (:r zappos) d-zp)
        (printf "    Euclidean ratio: %.1fx | Hyperbolic ratio: %.1fx%n"
                (/ (:r zappos) (max (:r allbirds) 0.001))
                (/ d-zp (max d-ab 0.001)))
        (println "    → Hyperbolic space amplifies the capability gap.")))

    "route"
    (let [category (second *command-line-args*)
          valid-shoes #{"A" "B" "D" "E" "H" "N" "Q" "S" "V" "Z"}
          valid-elec  #{"A" "B" "G" "S" "V"}
          valid (case category
                  "shoes"       valid-shoes
                  "electronics" valid-elec
                  (set (map :letter channels)))
          ranked (route-hyperbolic embedded valid)]
      (println (format "Hyperbolic routing for '%s':" (or category "all")))
      (println)
      (printf "  %-2s %-16s %-14s %8s  %s%n" "" "Channel" "Class" "d(0,z)" "UCP?")
      (doseq [ch ranked]
        (printf "  %-2s %-16s %-14s %8.4f  %s%n"
                (:letter ch) (:name ch) (name (:class ch))
                (hyperbolic-distance-from-origin [(:x ch) (:y ch)])
                (if (:ucp? ch) "★ LIVE" "")))
      (println)
      (let [best (first ranked)]
        (printf "  → Best: %s (%s) at hyperbolic distance %.4f%s%n"
                (:name best) (:letter best)
                (hyperbolic-distance-from-origin [(:x best) (:y best)])
                (if (:ucp? best) " [UCP LIVE]" ""))))

    "distances"
    (do
      (println "Pairwise hyperbolic distances (sample):")
      (println)
      (let [sample (take 6 (sort-by :r embedded))]
        (printf "  %16s" "")
        (doseq [ch sample] (printf " %8s" (:letter ch)))
        (println)
        (doseq [u sample]
          (printf "  %-2s %-12s" (:letter u) (:name u))
          (doseq [v sample]
            (printf " %8.3f" (poincare-distance [(:x u) (:y u)] [(:x v) (:y v)])))
          (println))))

    ;; default
    (do
      (println "poincare-commerce.bb — Hyperbolic embeddings for commerce channels")
      (println "  Nicol M. Kela: embed hierarchy into Poincaré disk")
      (println)
      (println "  embed              Show all 26 channels embedded in Poincaré disk")
      (println "  route <category>   Hyperbolic nearest-neighbor routing (shoes, electronics, all)")
      (println "  distances          Pairwise hyperbolic distance matrix (sample)"))))
