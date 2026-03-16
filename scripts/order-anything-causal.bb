#!/usr/bin/env bb
;; order-anything-causal.bb — Universal ordering via plurigrid/causal + ACP
;;
;; #5DC9F7's insight: don't study objects, study the morphisms between them.
;; A "shoe" and a "mattress" and a "human for hire" are all the same thing:
;;   Object in Ord — the category of orderable items
;;   Morphism: money × constraints → delivered(object)
;;   Service: a functor from Ord to the category of fulfillment
;;
;; GF(3) trit semantics (universal, not domain-specific):
;;   MINUS (-1) = validation  (verify, authenticate, inspect, measure)
;;   ERGODIC(0) = coordination (route, match, schedule, compare)
;;   PLUS  (+1) = generation  (order, dispatch, assemble, deliver)

(require '[babashka.fs :as fs]
         '[clojure.string :as str])

;; ════════════════════════════════════════════════════════════════
;; #91A9BB (#CCFE16 canonical) — color is the universal invariant
;; ════════════════════════════════════════════════════════════════

(def ^:const GOLDEN (unchecked-long 0x9E3779B97F4A7C15))
(def ^:const MIX1   (unchecked-long 0xBF58476D1CE4E5B9))
(def ^:const MIX2   (unchecked-long 0x94D049BB133111EB))

(defn splitmix64 [^long state]
  (let [s (unchecked-add state GOLDEN)
        z s
        z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 30)) MIX1)
        z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 27)) MIX2)
        z (bit-xor z (unsigned-bit-shift-right z 31))]
    [s z]))

(defn seed->color [^long seed]
  (let [[_ v] (splitmix64 seed)]
    (format "#%06X" (bit-and (Math/abs v) 0xFFFFFF))))

;; ════════════════════════════════════════════════════════════════
;; #04F15C (Agent Communication Protocol) — universal message envelope
;; ════════════════════════════════════════════════════════════════

(defn acp [method params]
  {"jsonrpc" "2.0"
   "method"  method
   "params"  params
   "id"      (str (System/currentTimeMillis))})

;; ════════════════════════════════════════════════════════════════
;; 26 UNIVERSAL SERVICE CHANNELS (A-Z)
;; These are functors from Ord → Fulfillment, not shoe-specific.
;; Each channel can deliver ANY orderable object.
;; ════════════════════════════════════════════════════════════════

(def channels
  [{:letter "A" :name "amazon"         :trit  1 :class :marketplace  :desc "Amazon Prime — universal marketplace, 1-2 day"}
   {:letter "B" :name "allbirds-ucp"   :trit -1 :class :ucp-live     :desc "Allbirds UCP LIVE — MCP checkout via Shopify, Google Pay (wide toe box shoes)"}
   {:letter "C" :name "cobbler-craft"  :trit  0 :class :artisan      :desc "Local artisan/craftsperson via referral"}
   {:letter "D" :name "delivery-app"   :trit  1 :class :same-day     :desc "Instacart/DoorDash/Shipt — same-day from local store"}
   {:letter "E" :name "ebay-resale"    :trit -1 :class :resale       :desc "eBay authenticated resale/pre-owned"}
   {:letter "F" :name "facebook-local" :trit  0 :class :p2p          :desc "FB Marketplace/Craigslist local pickup"}
   {:letter "G" :name "google-shop"    :trit  1 :class :aggregator   :desc "Google Shopping price comparison → route"}
   {:letter "H" :name "human-expert"   :trit -1 :class :rent-a-human :desc "TaskRabbit expert shopper/assembler/consultant"}
   {:letter "I" :name "ikea-deliver"   :trit  0 :class :furniture    :desc "IKEA delivery + optional assembly"}
   {:letter "J" :name "jet-courier"    :trit  1 :class :courier      :desc "Uber Connect / local courier service"}
   {:letter "K" :name "kit-subscribe"  :trit -1 :class :subscription :desc "Subscribe & save / auto-replenish"}
   {:letter "L" :name "liquidation"    :trit  0 :class :discount     :desc "Liquidation/overstock/clearance"}
   {:letter "M" :name "mercari-p2p"    :trit  1 :class :resale       :desc "Mercari/Poshmark peer-to-peer resale"}
   {:letter "N" :name "nordstrom-styl" :trit -1 :class :concierge    :desc "Concierge/stylist service (Nordstrom, etc.)"}
   {:letter "O" :name "offerup-local"  :trit  0 :class :p2p          :desc "OfferUp/Nextdoor local exchange"}
   {:letter "P" :name "prime-fresh"    :trit  1 :class :grocery      :desc "Amazon Fresh / Whole Foods — consumables"}
   {:letter "Q" :name "quick-errand"   :trit -1 :class :rent-a-human :desc "TaskRabbit quick errand — buy & deliver anything"}
   {:letter "R" :name "rei-coop"       :trit  0 :class :coop         :desc "REI/Costco — membership-based wholesale"}
   {:letter "S" :name "specialty-web"  :trit  1 :class :specialist   :desc "Domain-specific e-commerce (Wayfair, Zappos, etc.)"}
   {:letter "T" :name "target-shipt"   :trit -1 :class :same-day     :desc "Target via Shipt — same-day general goods"}
   {:letter "U" :name "used-thrift"    :trit  0 :class :thrift       :desc "ThriftUp/Goodwill/local thrift"}
   {:letter "V" :name "vendor-direct"  :trit  1 :class :direct       :desc "Vendor direct (any brand's own site)"}
   {:letter "W" :name "walmart-plus"   :trit -1 :class :mass-market  :desc "Walmart+ delivery — universal general"}
   {:letter "X" :name "xpress-local"   :trit  0 :class :same-day     :desc "Doordash Drive / local express pickup"}
   {:letter "Y" :name "yoox-luxury"    :trit  1 :class :luxury       :desc "YOOX/Farfetch/SSENSE — luxury/designer"}
   {:letter "Z" :name "zappos-return"  :trit -1 :class :free-return  :desc "Free-return strategy (order N sizes/variants)"}])

;; ════════════════════════════════════════════════════════════════
;; OBJECT CATEGORIES — what can be ordered
;; ════════════════════════════════════════════════════════════════

(def categories
  {:shoes      {:constraints ["size" "width" "toe_box" "heel" "arch"]
                :channels    #{"A" "B" "D" "E" "H" "N" "Q" "S" "V" "Z"}
                :ucp-live    #{"B"}}  ;; B=Allbirds: LIVE UCP MCP checkout
   :mattress   {:constraints ["size" "firmness" "material" "height"]
                :channels    #{"A" "B" "G" "I" "L" "S" "V"}}
   :furniture  {:constraints ["dimensions" "material" "color" "assembly"]
                :channels    #{"A" "F" "I" "L" "O" "S" "V" "W"}}
   :bedding    {:constraints ["size" "material" "thread_count"]
                :channels    #{"A" "D" "P" "S" "T" "V" "W"}}
   :lighting   {:constraints ["type" "wattage" "color_temp" "dimmable"]
                :channels    #{"A" "D" "I" "S" "T" "W"}}
   :appliance  {:constraints ["type" "capacity" "voltage" "energy_star"]
                :channels    #{"A" "B" "D" "G" "L" "R" "T" "V" "W"}}
   :kitchen    {:constraints ["material" "type" "dishwasher_safe"]
                :channels    #{"A" "D" "I" "R" "T" "W"}}
   :cleaning   {:constraints ["type" "scent" "eco"]
                :channels    #{"A" "D" "P" "T" "W"}}
   :bathroom   {:constraints ["type" "material" "color"]
                :channels    #{"A" "D" "I" "T" "W"}}
   :human      {:constraints ["skill" "duration" "location" "urgency"]
                :channels    #{"H" "N" "Q"}}
   :internet   {:constraints ["speed" "provider" "contract"]
                :channels    #{"B" "V"}}
   :electronics {:constraints ["part_no" "voltage" "qty" "package"]
                 :channels    #{"A" "B" "G" "S" "V"}}
   :mechanical  {:constraints ["material" "dimensions" "qty" "tolerance"]
                 :channels    #{"A" "B" "S" "V"}}
   :compute     {:constraints ["model" "ram" "storage" "form_factor"]
                 :channels    #{"A" "B" "G" "R" "S" "V"}}
   :wearable    {:constraints ["model" "size" "connectivity"]
                 :channels    #{"A" "B" "S" "V"}}
   :misc       {:constraints ["description"]
                :channels    (set (map :letter channels))}})

;; ════════════════════════════════════════════════════════════════
;; APARTMENT SETUP — canonical bedroom checklist
;; ════════════════════════════════════════════════════════════════

(def apartment-bedroom
  [{:item "mattress"          :cat :mattress   :pri 1 :constraints {"size" "Full/Queen" "firmness" "medium" "material" "hybrid/foam"}}
   {:item "bed-frame"         :cat :furniture  :pri 1 :constraints {"size" "matching mattress" "material" "wood/metal" "assembly" "required"}}
   {:item "sheets"            :cat :bedding    :pri 1 :constraints {"size" "matching mattress" "material" "cotton/bamboo" "thread_count" "300+"}}
   {:item "pillows"           :cat :bedding    :pri 1 :constraints {"material" "memory-foam/down-alt" "count" "2"}}
   {:item "comforter"         :cat :bedding    :pri 2 :constraints {"size" "matching mattress" "material" "down-alt" "season" "all-season"}}
   {:item "desk"              :cat :furniture  :pri 2 :constraints {"dimensions" "48x24 min" "material" "wood/metal"}}
   {:item "desk-chair"        :cat :furniture  :pri 2 :constraints {"type" "ergonomic" "adjustable" "height,armrest"}}
   {:item "lamp-desk"         :cat :lighting   :pri 2 :constraints {"type" "LED" "dimmable" "yes" "color_temp" "adjustable"}}
   {:item "lamp-floor"        :cat :lighting   :pri 3 :constraints {"type" "LED" "dimmable" "yes"}}
   {:item "curtains"          :cat :furniture  :pri 2 :constraints {"type" "blackout" "dimensions" "measure-window"}}
   {:item "towels"            :cat :bathroom   :pri 1 :constraints {"type" "bath+hand" "count" "4" "material" "cotton"}}
   {:item "trash-can"         :cat :misc       :pri 1 :constraints {"description" "small bedroom + bathroom"}}
   {:item "hangers"           :cat :misc       :pri 2 :constraints {"description" "20-30 non-slip"}}
   {:item "shoes-allbirds"    :cat :shoes      :pri 2 :constraints {"size" "7.5W" "width" "wide" "toe_box" "wide" "heel" "attached" "ucp" "allbirds.com/api/ucp/mcp"}}
   {:item "cleaning-supplies" :cat :cleaning   :pri 1 :constraints {"type" "all-purpose,glass,bathroom" "eco" "preferred"}}
   {:item "kitchen-basics"    :cat :kitchen    :pri 2 :constraints {"type" "pot,pan,utensils,plates,cups" "material" "stainless/ceramic"}}
   {:item "power-strip"       :cat :appliance  :pri 1 :constraints {"type" "surge-protector" "outlets" "6+"}}
   {:item "wifi-setup"        :cat :internet   :pri 1 :constraints {"speed" "100mbps+" "provider" "local"}}
   {:item "assembly-human"    :cat :human      :pri 1 :constraints {"skill" "furniture-assembly" "duration" "3-4hr" "urgency" "same-week"}}
   {:item "deep-clean-human"  :cat :human      :pri 1 :constraints {"skill" "deep-cleaning" "duration" "2-3hr" "urgency" "move-in-day"}}])

;; ════════════════════════════════════════════════════════════════
;; HERO PILL DISPENSER — hardware bill of materials
;;
;; Extracted from firmware: hero_motor_control.rs (steppers, vacuum),
;; hero_load_cell.rs (HX711, load cells), hero_optical_sensor.rs (IR pairs),
;; hero_power_management.rs (PSU, battery, ESP32), hero_wifi_mqtt.rs (ESP32 WiFi),
;; hero_serial_console.rs (USB debug), hero_safety_interlock.rs (watchdog, sensors),
;; hero_key_management.rs (ESP32 eFuse), HeroWatch/*.swift (Apple Watch).
;;
;; Price estimates in USD. Priority: 1=blocking, 2=core, 3=nice-to-have.
;; ════════════════════════════════════════════════════════════════

(def hero-bom
  [;; --- MCU & Compute (pri 1) ---
   {:item "esp32-s3-devkit"     :cat :electronics :pri 1 :est-usd 10
    :constraints {"part_no" "ESP32-S3-DevKitC-1" "voltage" "3.3V" "qty" "1" "package" "devkit"}}
   {:item "raspberry-pi"        :cat :compute     :pri 1 :est-usd 60
    :constraints {"model" "Pi 4B/5" "ram" "4GB+" "storage" "32GB microSD" "form_factor" "SBC"}}
   {:item "microsd-card"        :cat :electronics :pri 1 :est-usd 10
    :constraints {"part_no" "any" "voltage" "3.3V" "qty" "1" "package" "32GB+ Class 10"}}

   ;; --- Stepper Motors & Drivers (pri 1) ---
   {:item "nema17-stepper"      :cat :mechanical  :pri 1 :est-usd 36
    :constraints {"material" "steel/plastic" "dimensions" "42x42x40mm 200step/rev" "qty" "3" "tolerance" "1.8deg/step"}}
   {:item "tmc2209-driver"      :cat :electronics :pri 1 :est-usd 18
    :constraints {"part_no" "TMC2209" "voltage" "4.75-29V" "qty" "3" "package" "StepStick"}}

   ;; --- Load Cells & ADC (pri 1) ---
   {:item "hx711-adc"           :cat :electronics :pri 1 :est-usd 10
    :constraints {"part_no" "HX711" "voltage" "2.6-5.5V" "qty" "5" "package" "breakout"}}
   {:item "load-cell-50g"       :cat :electronics :pri 1 :est-usd 25
    :constraints {"part_no" "TAL220/CZL635" "voltage" "5V excitation" "qty" "5" "package" "50g half-bridge"}}

   ;; --- Optical Sensors (pri 1) ---
   {:item "ir-emitter-detector" :cat :electronics :pri 1 :est-usd 8
    :constraints {"part_no" "TCPT1300X01/GP1A57HRJ00F" "voltage" "3.3V" "qty" "5 pairs" "package" "through-hole/SMD"}}

   ;; --- Vacuum Pump (pri 1) ---
   {:item "vacuum-pump-mini"    :cat :mechanical  :pri 1 :est-usd 12
    :constraints {"material" "plastic/DC motor" "dimensions" "27x22mm" "qty" "1" "tolerance" "-30kPa min"}}

   ;; --- Linear Motion (pri 2) ---
   {:item "lead-screw-200mm"    :cat :mechanical  :pri 2 :est-usd 20
    :constraints {"material" "stainless steel" "dimensions" "T8x200mm 2mm pitch" "qty" "2" "tolerance" "Tr8x2"}}
   {:item "linear-rail-200mm"   :cat :mechanical  :pri 2 :est-usd 18
    :constraints {"material" "steel" "dimensions" "MGN12H 200mm" "qty" "2" "tolerance" "C/H class"}}
   {:item "linear-bearing"      :cat :mechanical  :pri 2 :est-usd 8
    :constraints {"material" "steel" "dimensions" "MGN12H carriage" "qty" "2" "tolerance" "standard"}}

   ;; --- Power (pri 1) ---
   {:item "psu-12v-5a"          :cat :electronics :pri 1 :est-usd 12
    :constraints {"part_no" "any" "voltage" "12V/5A 60W" "qty" "1" "package" "barrel jack"}}
   {:item "buck-converter"      :cat :electronics :pri 1 :est-usd 4
    :constraints {"part_no" "LM2596/MP1584" "voltage" "12V→3.3V/5V" "qty" "2" "package" "adjustable module"}}
   {:item "lipo-battery"        :cat :electronics :pri 2 :est-usd 15
    :constraints {"part_no" "any" "voltage" "3.7V 2000mAh" "qty" "1" "package" "JST-PH"}}

   ;; --- Connectivity (pri 1) ---
   {:item "usb-c-cable"         :cat :electronics :pri 1 :est-usd 5
    :constraints {"part_no" "any" "voltage" "5V" "qty" "2" "package" "USB-C data+power"}}

   ;; --- Carousel & Canisters (pri 1) ---
   {:item "carousel-turntable"  :cat :mechanical  :pri 1 :est-usd 25
    :constraints {"material" "PLA/PETG 3D-print or CNC" "dimensions" "10-slot 160mm OD" "qty" "1" "tolerance" "0.2mm layer"}}
   {:item "pill-canister"       :cat :mechanical  :pri 1 :est-usd 10
    :constraints {"material" "PETG/medical-grade" "dimensions" "30mm OD x 80mm" "qty" "5" "tolerance" "food-safe"}}
   {:item "enclosure"           :cat :mechanical  :pri 2 :est-usd 30
    :constraints {"material" "PLA/PETG 3D-print" "dimensions" "250x200x180mm" "qty" "1" "tolerance" "0.2mm layer"}}

   ;; --- Wearable (pri 2) ---
   {:item "apple-watch"         :cat :wearable    :pri 2 :est-usd 250
    :constraints {"model" "SE 2nd gen or newer" "size" "40/44mm" "connectivity" "WiFi+BT"}}

   ;; --- Breakout & Wiring (pri 1) ---
   {:item "breadboard"          :cat :electronics :pri 1 :est-usd 5
    :constraints {"part_no" "any" "voltage" "n/a" "qty" "2" "package" "830-point"}}
   {:item "jumper-wire-kit"     :cat :electronics :pri 1 :est-usd 7
    :constraints {"part_no" "any" "voltage" "n/a" "qty" "120pc M-M/M-F/F-F" "package" "dupont"}}
   {:item "header-pins"         :cat :electronics :pri 1 :est-usd 3
    :constraints {"part_no" "any" "voltage" "n/a" "qty" "5 strips 40pin" "package" "2.54mm"}}

   ;; --- MQTT Broker (software, but needs host) ---
   {:item "mosquitto-setup"     :cat :compute     :pri 1 :est-usd 0
    :constraints {"model" "Eclipse Mosquitto" "ram" "n/a" "storage" "apt install" "form_factor" "software"}}

   ;; --- Assembly Human (pri 3) ---
   {:item "assembly-human-hero" :cat :human       :pri 3 :est-usd 80
    :constraints {"skill" "electronics-assembly,soldering" "duration" "2-3hr" "location" "local" "urgency" "same-week"}}])

(def ORDER_DIR "/tmp/universal-orders")
(fs/create-dirs ORDER_DIR)

;; ════════════════════════════════════════════════════════════════
;; ROUTING — find best channel for an item given its category
;; ════════════════════════════════════════════════════════════════

(defn route-item [{:keys [item cat constraints pri]}]
  (let [cat-info   (get categories cat (get categories :misc))
        valid-chs  (:channels cat-info)
        matched    (filter #(valid-chs (:letter %)) channels)
        ;; prefer PLUS for generation/ordering
        sorted     (sort-by #(- (:trit %)) matched)]
    {:item item :category cat :priority pri :constraints constraints
     :channels (mapv #(select-keys % [:letter :name :trit :class :desc]) sorted)}))

(defn best-channel [routed]
  (first (:channels routed)))

;; ════════════════════════════════════════════════════════════════
;; ORDER PLACEMENT — universal
;; ════════════════════════════════════════════════════════════════

(defn place-order [item-map channel-override]
  (let [routed  (route-item item-map)
        channel (if channel-override
                  (first (filter #(= channel-override (:letter %)) channels))
                  (best-channel routed))
        color   (seed->color (hash (str (:item item-map) (:name channel))))
        order   {:id          (str (System/currentTimeMillis))
                 :item        (:item item-map)
                 :category    (name (:cat item-map))
                 :channel     channel
                 :constraints (:constraints item-map)
                 :priority    (:pri item-map)
                 :color       color
                 :timestamp   (str (java.time.Instant/now))
                 :acp         (acp "order/place"
                                {"item"        (:item item-map)
                                 "category"    (name (:cat item-map))
                                 "channel"     (:name channel)
                                 "constraints" (:constraints item-map)})}
        path    (str ORDER_DIR "/" (:item item-map) "-" (:id order) ".json")]
    (spit path (pr-str order))
    order))

;; ════════════════════════════════════════════════════════════════
;; CLI
;; ════════════════════════════════════════════════════════════════

(let [args *command-line-args*
      cmd  (first args)]
  (case cmd

    "channels"
    (do
      (println "╔═══════════════════════════════════════════════════════════════════╗")
      (println "║  UNIVERSAL ORDER — 26 CHANNELS (Ord → Fulfillment functors)     ║")
      (println "╚═══════════════════════════════════════════════════════════════════╝")
      (println)
      (doseq [c channels]
        (let [tname (get {-1 "MINUS" 0 "ERGODIC" 1 "PLUS"} (:trit c))
              color (seed->color (hash (:name c)))]
          (printf "  %s %+2d %-8s %-10s %s %-16s %s%n"
                  (:letter c) (:trit c) tname (name (:class c)) color (:name c) (:desc c))))
      (println)
      (let [plus  (count (filter #(= 1 (:trit %)) channels))
            zero  (count (filter #(= 0 (:trit %)) channels))
            minus (count (filter #(= -1 (:trit %)) channels))]
        (printf "  GF(3): PLUS=%d ERGODIC=%d MINUS=%d | Σ=%d ✓%n" plus zero minus
                (reduce + (map :trit channels)))))

    "categories"
    (do
      (println "╔═══════════════════════════════════════════════════════════════════╗")
      (println "║  OBJECT CATEGORIES — what can be ordered                        ║")
      (println "╚═══════════════════════════════════════════════════════════════════╝")
      (println)
      (doseq [[k v] (sort-by key categories)]
        (printf "  %-12s constraints: %-40s channels: %s%n"
                (name k) (str/join ", " (:constraints v))
                (str/join "" (sort (:channels v))))))

    "bedroom"
    (do
      (println "╔═══════════════════════════════════════════════════════════════════╗")
      (println "║  APARTMENT BEDROOM SETUP — routed through universal channels    ║")
      (println "╚═══════════════════════════════════════════════════════════════════╝")
      (println)
      (let [routed (map route-item apartment-bedroom)
            by-pri (group-by :priority routed)]
        (doseq [pri (sort (keys by-pri))]
          (printf "  ── Priority %d ──────────────────────────────────────────────%n" pri)
          (doseq [r (get by-pri pri)]
            (let [best (best-channel r)
                  color (seed->color (hash (str (:item r) (:name best))))]
              (printf "    %s %-20s → %s %-16s [%s] %s%n"
                      color (:item r)
                      (:letter best) (:name best)
                      (name (:category r))
                      (str/join ", " (map (fn [[k v]] (str k "=" v)) (:constraints r))))))
          (println))))

    "hero"
    (do
      (println "╔═══════════════════════════════════════════════════════════════════╗")
      (println "║  HERO PILL DISPENSER — hardware BOM routed through channels     ║")
      (println "╚═══════════════════════════════════════════════════════════════════╝")
      (println)
      (let [routed (map route-item hero-bom)
            by-pri (group-by :priority routed)
            total  (reduce + (map :est-usd hero-bom))]
        (doseq [pri (sort (keys by-pri))]
          (printf "  ── Priority %d ──────────────────────────────────────────────%n" pri)
          (doseq [r (get by-pri pri)]
            (let [best  (best-channel r)
                  bom   (first (filter #(= (:item %) (:item r)) hero-bom))
                  color (seed->color (hash (str (:item r) (:name best))))]
              (printf "    %s $%-4d %-22s → %s %-16s [%s]%n"
                      color (:est-usd bom) (:item r)
                      (:letter best) (:name best)
                      (name (:category r)))))
          (println))
        (printf "  Total estimated BOM: $%d (%d items)%n" total (count hero-bom))
        (printf "  (excludes Apple Watch if already owned)%n")))

    "hero-order-all"
    (do
      (println "╔═══════════════════════════════════════════════════════════════════╗")
      (println "║  HERO ORDER ALL — dispatching full hardware BOM                 ║")
      (println "╚═══════════════════════════════════════════════════════════════════╝")
      (println)
      (let [orders (mapv #(place-order % nil) hero-bom)]
        (doseq [o orders]
          (let [bom (first (filter #(= (:item %) (:item o)) hero-bom))]
            (printf "  %s $%-4d %-22s → %s %-16s [pri=%d]%n"
                    (:color o) (:est-usd bom) (:item o)
                    (get-in o [:channel :letter]) (get-in o [:channel :name])
                    (:priority o))))
        (println)
        (let [total (reduce + (map :est-usd hero-bom))]
          (printf "  Total: %d items, $%d estimated, %d ACP messages%n"
                  (count orders) total (count orders)))
        (let [manifest {:timestamp (str (java.time.Instant/now))
                        :count     (count orders)
                        :bom       "hero-pill-dispenser"
                        :items     (mapv #(select-keys % [:item :category :channel :color :priority]) orders)}
              mpath    (str ORDER_DIR "/hero-manifest.json")]
          (spit mpath (pr-str manifest))
          (printf "  Manifest: %s%n" mpath))))

    "order"
    (let [item-name (second args)
          channel   (nth args 2 nil)
          all-items (concat apartment-bedroom hero-bom)
          item-map  (first (filter #(= item-name (:item %)) all-items))]
      (if item-map
        (let [order (place-order item-map channel)]
          (printf "  %s %-20s → %s via %s%n"
                  (:color order) (:item order)
                  (get-in order [:channel :letter]) (get-in order [:channel :name]))
          (printf "       ACP: order/place dispatched → %s%n" (str ORDER_DIR "/" (:item order) "-" (:id order) ".json")))
        (println (str "  Item not found: " item-name
                      "\n  Bedroom: " (str/join ", " (map :item apartment-bedroom))
                      "\n  Hero BOM: " (str/join ", " (map :item hero-bom))))))

    "order-all"
    (do
      (println "╔═══════════════════════════════════════════════════════════════════╗")
      (println "║  ORDER ALL — dispatching entire bedroom setup                   ║")
      (println "╚═══════════════════════════════════════════════════════════════════╝")
      (println)
      (let [orders (mapv #(place-order % nil) apartment-bedroom)]
        (doseq [o orders]
          (printf "  %s %-20s → %s %-16s [pri=%d]%n"
                  (:color o) (:item o)
                  (get-in o [:channel :letter]) (get-in o [:channel :name])
                  (:priority o)))
        (println)
        (printf "  Total: %d items ordered, %d ACP messages generated%n"
                (count orders) (count orders))
        (let [manifest {:timestamp (str (java.time.Instant/now))
                        :count     (count orders)
                        :items     (mapv #(select-keys % [:item :category :channel :color :priority]) orders)}
              mpath    (str ORDER_DIR "/manifest.json")]
          (spit mpath (pr-str manifest))
          (printf "  Manifest: %s%n" mpath))))

    "nash"
    (do
      (println "#502881 equilibrium for UNIVERSAL ordering:")
      (println "  Commodity (<$50):     A or W or T")
      (println "  Mid-range ($50-300):  S or V")
      (println "  Premium ($300+):      N or Y")
      (println "  Urgent same-day:      D or X")
      (println "  Need a human:         H or Q")
      (println "  Furniture+assembly:   I + H")
      (println "  Try-before-buy:       Z (order variants, free return)")
      (println "  Budget:               F > U > L")
      (println)
      (println "  Hero electronics:     S (Adafruit/SparkFun/Digikey) or V (vendor direct)")
      (println "  Hero mechanical:      S (McMaster/Misumi) or A (Amazon)")
      (println "  Hero 3D-printed:      S (PCBWay/JLCPCB/local makerspace)")
      (println "  Hero compute (RPi):   A or R (Micro Center)")
      (println)
      (println "  UCP LIVE MERCHANTS:")
      (println "  Wide toe box shoes:   B (Allbirds UCP — MCP at allbirds.com/api/ucp/mcp)")
      (println "    → Google Pay / Shopify Card, direct agent checkout, no browser needed")
      (println "    → Tree Runners, Wool Runners: wide toe box, zero-drop, ships to hotels"))

    ;; default
    (do
      (println "order-anything-causal.bb — Universal ordering via plurigrid/causal + ACP")
      (println)
      (println "  channels          26 universal service channels with GF(3) color")
      (println "  categories        Object categories and their valid channels")
      (println "  bedroom           Route apartment bedroom checklist to best channels")
      (println "  hero              Route Hero pill dispenser hardware BOM to channels")
      (println "  hero-order-all    Order entire Hero hardware BOM")
      (println "  order <item> [ch] Place order for any item (bedroom or hero)")
      (println "  order-all         Order entire bedroom setup")
      (println "  nash              #502881 equilibrium tiers for universal ordering"))))
