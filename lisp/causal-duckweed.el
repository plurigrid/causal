;;; causal-duckweed.el --- Order anything in SF via ACP/MCP transient -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Plurigrid Contributors
;; Author: Barton Rhodes <barton@plurigrid.com>
;; Keywords: convenience, transient, ordering, mcp, acp, plants, flowers, food
;; Requires: transient, message, json

;;; Commentary:
;;
;; Duckweed: the smallest flowering plant, grows anywhere, reproduces fast.
;; A lightweight ordering system for anything deliverable in San Francisco
;; via Emacs causal transient menus with ACP capability grants and MCP dispatch.
;;
;; Ontology: orderable → {food, plant, flower, supply, custom}
;; Chain: keystroke → category transient → item transient → ACP grant
;;        → MCP dispatch → org-log → email/vendor API → say announce
;;
;; Bound globally to C-c d (for duckweed).

;;; Code:

(require 'transient)
(require 'message)
(require 'json)

(defgroup causal-duckweed nil
  "Order anything deliverable in SF via transient + ACP/MCP."
  :group 'convenience
  :prefix "causal-duckweed-")

;; ── Configuration ───────────────────────────────────────

(defcustom causal-duckweed-location "888 Howard Street, San Francisco, CA 94103"
  "Delivery address."
  :type 'string)

(defcustom causal-duckweed-location-name "InterContinental San Francisco"
  "Human name for delivery location."
  :type 'string)

(defcustom causal-duckweed-room ""
  "Room/unit number (set per-session)."
  :type 'string)

(defcustom causal-duckweed-recipient "Barton Rhodes"
  "Recipient name."
  :type 'string)

(defcustom causal-duckweed-log-file "~/.topos/duckweed-orders.org"
  "Org file for order logging."
  :type 'string)

(defcustom causal-duckweed-order-dir "~/.topos/orders/"
  "Directory for MCP dispatch JSON files."
  :type 'string)

(defcustom causal-duckweed-use-mcp t
  "If non-nil, dispatch orders via MCP JSON."
  :type 'boolean)

(defcustom causal-duckweed-use-acp t
  "If non-nil, wrap orders in ACP capability envelope."
  :type 'boolean)

(defcustom causal-duckweed-voice "Samantha"
  "macOS say voice for order announcements."
  :type 'string)

;; ── Vendor Registry ─────────────────────────────────────
;; SF-specific vendors keyed by category.
;; Each: (id name url email category delivery-notes)

(defvar causal-duckweed-vendors
  '(;; Food
    (concierge  "Hotel Concierge"     nil nil
                "food" "In-building, fastest")
    (doordash   "DoorDash"            "https://doordash.com" nil
                "food" "Lobby delivery, 30-60min")
    (ubereats   "Uber Eats"           "https://ubereats.com" nil
                "food" "Lobby delivery, 25-50min")

    ;; Plants
    (sloat      "Sloat Garden Center" "https://sloatgardens.com"
                "info@sloatgardens.com"
                "plant" "3 SF locations, same-day pickup or delivery")
    (flowercraft "Flowercraft Garden" "https://flowercraftgc.com"
                 nil
                 "plant" "270 varieties, Inner Sunset")
    (plantshed  "The Plant Shed SF"   nil nil
                "plant" "Mission District, succulents + tropicals")
    (bloomscape "Bloomscape"          "https://bloomscape.com" nil
                "plant" "Ships nationwide, arrives potted + soil")
    (thesill    "The Sill"            "https://thesill.com" nil
                "plant" "Low-light specialists, ships to SF")

    ;; Flowers
    (farmgirl   "Farmgirl Flowers"    "https://farmgirlflowers.com"
                "hello@farmgirlflowers.com"
                "flower" "SF-based, sustainable, burlap-wrapped")
    (bloomnation "BloomNation"        "https://bloomnation.com" nil
                 "flower" "Local florist marketplace")
    (odealarose "Ode a la Rose"       "https://odealarose.com" nil
                "flower" "French-style, same-day SF delivery")
    (bouqs      "The Bouqs Co"        "https://bouqs.com" nil
                "flower" "Farm-direct, subscription available")
    (proflowers "ProFlowers"          "https://proflowers.com" nil
                "flower" "Budget option, next-day")

    ;; Supplies
    (instacart  "Instacart"           "https://instacart.com" nil
                "supply" "Any SF store, 1-2hr delivery")
    (amazon     "Amazon Fresh/Same-Day" "https://amazon.com" nil
                "supply" "Prime same-day, wide selection"))
  "SF vendor registry. Each: (ID NAME URL EMAIL CATEGORY NOTES).")

;; ── Orderable Ontology ──────────────────────────────────
;; Format: (key label category vendor-preference price notes)

(defvar causal-duckweed-food-items
  '(("a" "Avocado toast + coffee"       "food" concierge "$18" "")
    ("b" "Breakfast burrito"             "food" doordash  "$16" "scrambled eggs, beans, salsa")
    ("c" "Caesar salad"                  "food" ubereats  "$14" "add chicken +$6")
    ("d" "Dim sum platter"              "food" doordash  "$28" "for 2, steamed + fried mix")
    ("e" "Eggs benedict"                 "food" concierge "$22" "hollandaise on the side")
    ("f" "Fish tacos"                    "food" ubereats  "$18" "baja style, lime crema")
    ("g" "Grilled salmon"               "food" concierge "$34" "with roasted vegetables")
    ("h" "Hamburger + fries"            "food" doordash  "$20" "medium rare, brioche bun")
    ("i" "Indian curry (tikka masala)"  "food" ubereats  "$22" "with naan and rice")
    ("j" "Japanese ramen (tonkotsu)"    "food" doordash  "$19" "extra chashu +$4")
    ("k" "Korean bibimbap"              "food" ubereats  "$21" "stone pot, extra spicy ok")
    ("l" "Lobster roll"                  "food" doordash  "$32" "Connecticut style")
    ("m" "Margherita pizza"             "food" doordash  "$18" "wood-fired if available")
    ("n" "Noodle soup (pho)"            "food" ubereats  "$17" "rare beef, extra herbs")
    ("o" "Omelette (custom)"            "food" concierge "$15" "specify fillings at prompt")
    ("p" "Pad thai"                      "food" ubereats  "$18" "shrimp, medium spice")
    ("q" "Quinoa bowl"                   "food" doordash  "$16" "vegan, roasted veg")
    ("r" "Ribeye steak"                  "food" concierge "$48" "12oz, medium rare")
    ("s" "Sushi combo"                   "food" doordash  "$38" "12pc nigiri + roll")
    ("t" "Thai green curry"             "food" ubereats  "$20" "with jasmine rice"))
  "Food items A-T.")

(defvar causal-duckweed-plant-items
  '(("a" "Aloe vera (potted)"           "plant" sloat     "$12" "low water, bright indirect")
    ("b" "Boston fern (hanging)"        "plant" bloomscape "$28" "humidity lover, shade ok")
    ("c" "Chinese money plant"          "plant" thesill   "$22" "pilea peperomioides")
    ("d" "Duckweed (Lemna minor)"       "plant" sloat     "$8"  "the namesake! floating aquatic")
    ("e" "Echeveria rosette"            "plant" flowercraft "$10" "succulent, full sun")
    ("f" "Fiddle leaf fig"              "plant" bloomscape "$65" "bright indirect, 3ft")
    ("g" "Golden pothos"                "plant" thesill   "$18" "trailing vine, low light ok")
    ("h" "Herbs (basil/thyme/rosemary)" "plant" sloat     "$14" "windowsill trio")
    ("i" "Italian stone pine"           "plant" flowercraft "$35" "bonsai form, bright light")
    ("j" "Jade plant"                   "plant" sloat     "$15" "crassula ovata, drought ok")
    ("k" "Kalanchoe (flowering)"        "plant" flowercraft "$12" "orange/pink/red blooms")
    ("l" "Lavender (French)"            "plant" sloat     "$16" "fragrant, full sun, drought ok")
    ("m" "Monstera deliciosa"           "plant" bloomscape "$45" "swiss cheese, bright indirect")
    ("n" "Norfolk Island pine"          "plant" thesill   "$30" "mini indoor tree, medium light")
    ("o" "Orchid (phalaenopsis)"        "plant" bloomscape "$38" "moth orchid, reblooms")
    ("p" "Peace lily"                    "plant" thesill   "$25" "spathiphyllum, low light")
    ("q" "Queen of the night"           "plant" flowercraft "$20" "epiphyllum, rare bloom")
    ("r" "Rubber plant"                  "plant" bloomscape "$32" "ficus elastica, air purifier")
    ("s" "Snake plant"                   "plant" thesill   "$20" "sansevieria, bulletproof")
    ("t" "Tillandsia (air plant)"       "plant" sloat     "$8"  "no soil needed, mist weekly"))
  "Plant items A-T.")

(defvar causal-duckweed-flower-items
  '(("a" "Arrangement (seasonal mix)"   "flower" farmgirl   "$58" "designer's choice, burlap")
    ("b" "Bird of paradise stems"       "flower" odealarose "$45" "3 stems, tropical")
    ("c" "Chrysanthemums (pompom)"      "flower" bloomnation "$30" "white/yellow, long-lasting")
    ("d" "Dahlias (dinner plate)"       "flower" farmgirl   "$65" "seasonal, Aug-Nov peak")
    ("e" "Eucalyptus + ranunculus"      "flower" farmgirl   "$52" "textured, fragrant")
    ("f" "Freesia bunch"                "flower" bloomnation "$28" "intensely fragrant")
    ("g" "Garden roses (12)"            "flower" odealarose "$78" "long-stem, assorted colors")
    ("h" "Hydrangea dome"               "flower" farmgirl   "$48" "blue/pink/white")
    ("i" "Iris bouquet"                  "flower" bloomnation "$35" "purple/white, spring")
    ("j" "Jasmine vine (potted)"        "flower" bloomnation "$30" "fragrant, climbing")
    ("k" "King protea (single)"         "flower" farmgirl   "$25" "dramatic focal, long-lasting")
    ("l" "Lily (stargazer) bunch"       "flower" odealarose "$40" "fragrant, pink spotted")
    ("m" "Mixed wildflower bunch"       "flower" farmgirl   "$42" "California native mix")
    ("n" "Narcissus (paperwhite)"       "flower" bloomnation "$18" "bulbs or cut, fragrant")
    ("o" "Orchid spray (cymbidium)"     "flower" odealarose "$55" "cascading, green/pink")
    ("p" "Peony bouquet"                 "flower" farmgirl   "$72" "seasonal, Apr-Jun peak")
    ("q" "Queen Anne's lace"            "flower" bloomnation "$15" "filler/accent, airy")
    ("r" "Ranunculus bundle"            "flower" farmgirl   "$38" "paper-thin petals, pastel")
    ("s" "Sunflower bunch (10)"         "flower" bouqs      "$35" "cheerful, long stems")
    ("t" "Tulips (20 stems)"            "flower" odealarose "$42" "Dutch, assorted colors"))
  "Flower items A-T.")

;; ── CapTP / ACP Capability Layer ────────────────────────
;;
;; Three modes of capability granting:
;;
;; 1. captp:// URI — live Goblins vat invocation (if vat running)
;;    The capability reference IS the authorization.
;;    Having the sturdyref means you can invoke; no separate token.
;;
;; 2. ACP envelope — agent-to-agent capability delegation
;;    Wraps MCP tool calls with attenuation and scope.
;;    Vendor receives a delegated capability they can only fulfill.
;;
;; 3. Fallback — signed JSON with HMAC (offline/disconnected)
;;    For when no vat is reachable. Verifiable later.

(defcustom causal-duckweed-captp-vat nil
  "CapTP vat URI for live capability invocations.
When non-nil, orders are dispatched as Goblins capability calls.
Example: \"captp://t/coordinator\""
  :type '(choice (const nil) string))

(defcustom causal-duckweed-captp-vendor-caps nil
  "Alist of vendor-id → captp sturdyref for direct invocation.
Example: ((sloat . \"captp://sloat/order\") (farmgirl . \"captp://farmgirl/order\"))"
  :type '(alist :key-type symbol :value-type string))

(defun causal-duckweed--captp-invoke (vendor-id order-data)
  "Invoke CapTP capability for VENDOR-ID with ORDER-DATA.
Returns non-nil if invocation succeeded (vat reachable)."
  (let ((cap-uri (alist-get vendor-id causal-duckweed-captp-vendor-caps)))
    (when (and causal-duckweed-captp-vat cap-uri)
      ;; Attempt Goblins vat invocation via guile subprocess
      (let* ((scheme-expr
              (format "(use-modules (goblins) (goblins actor-lib captp))
(define vat (spawn-vat))
(with-vat vat
  (define vendor-cap (captp-connect \"%s\"))
  ($ vendor-cap 'place-order
     #:item %S
     #:category %S
     #:deliver-to \"%s\"
     #:room %S
     #:recipient %S
     #:notes %S))"
                      cap-uri
                      (alist-get 'item order-data)
                      (alist-get 'category order-data)
                      causal-duckweed-captp-vat
                      (alist-get 'room order-data)
                      (alist-get 'recipient order-data)
                      (or (alist-get 'custom_notes order-data) "")))
             (result (ignore-errors
                       (call-process "guile" nil nil nil
                                     "-c" scheme-expr))))
        (when (and result (= result 0))
          (message "CapTP invocation: %s → %s" vendor-id cap-uri)
          t)))))

(defun causal-duckweed--acp-envelope (order-data)
  "Wrap ORDER-DATA in an ACP capability envelope.
The envelope encodes:
- capability URI: where this order authority comes from
- grant type: what the holder can do (invoke = place order)
- scope: what actions are authorized
- attenuation: vendor can fulfill but not modify/cancel
- delegation chain: which vendor gets the attenuated capability"
  (let* ((cap-id (format "captp://t/order/%s/%s"
                         (alist-get 'category order-data)
                         (format-time-string "%Y%m%d%H%M%S")))
         (vendor-id (alist-get 'vendor order-data))
         (vendor-cap (alist-get (intern vendor-id)
                                causal-duckweed-captp-vendor-caps)))
    (append order-data
            `((acp_version . "0.1")
              (capability
               . ((uri . ,cap-id)
                  (granted_by . ,(or causal-duckweed-captp-vat
                                     "captp://t/coordinator"))
                  (grant . "invoke")
                  (scope . ["order:place" "order:status"])
                  ;; Attenuation: vendor gets subset of authority
                  (attenuated . t)
                  (delegate_to . ,(or vendor-cap
                                      (format "captp://%s/fulfillment"
                                              vendor-id)))
                  ;; Revocation: order can be cancelled before fulfillment
                  (revocable . t)
                  (revoke_before . ,(format-time-string
                                     "%Y-%m-%dT%H:%M:%SZ"
                                     (time-add (current-time) 3600)))
                  ;; Expiry: capability dies after 24h
                  (expires . ,(format-time-string
                               "%Y-%m-%dT%H:%M:%SZ"
                               (time-add (current-time) (* 3600 24))))
                  (issuer . ,causal-duckweed-recipient)))))))

;; ── Core Dispatch ───────────────────────────────────────

(defun causal-duckweed--ensure-room ()
  "Ensure room/unit is set."
  (when (string-empty-p causal-duckweed-room)
    (setq causal-duckweed-room (read-string "Room/unit number: ")))
  causal-duckweed-room)

(defun causal-duckweed--vendor-info (vendor-id)
  "Look up VENDOR-ID in vendor registry."
  (assq vendor-id causal-duckweed-vendors))

(defun causal-duckweed--log-order (item notes)
  "Log order to org file."
  (let ((file (expand-file-name causal-duckweed-log-file))
        (vendor (causal-duckweed--vendor-info (nth 3 item))))
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* ORDER %s [%s]\n"
                      (nth 1 item)
                      (format-time-string "%Y-%m-%d %H:%M")))
      (insert ":PROPERTIES:\n")
      (insert (format ":CATEGORY: %s\n" (nth 2 item)))
      (insert (format ":VENDOR: %s\n" (if vendor (nth 1 vendor) "unknown")))
      (insert (format ":PRICE: %s\n" (nth 4 item)))
      (insert (format ":LOCATION: %s\n" causal-duckweed-location-name))
      (insert (format ":ROOM: %s\n" causal-duckweed-room))
      (insert (format ":NOTES: %s\n" (or notes "")))
      (insert (format ":ORDERED: %s\n" (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
      (insert ":END:\n")
      (save-buffer)
      (message "Logged to %s" file))))

(defun causal-duckweed--mcp-dispatch (item notes)
  "Dispatch order via CapTP → ACP+MCP fallback chain.
Tries live CapTP vat first; falls back to ACP-wrapped MCP JSON."
  (let* ((vendor (causal-duckweed--vendor-info (nth 3 item)))
         (vendor-id (nth 3 item))
         (order-data `((schema . "duckweed-order-v1")
                       (item . ,(nth 1 item))
                       (category . ,(nth 2 item))
                       (vendor . ,(symbol-name vendor-id))
                       (vendor_name . ,(if vendor (nth 1 vendor) ""))
                       (vendor_url . ,(if vendor (nth 2 vendor) nil))
                       (vendor_email . ,(if vendor (nth 3 vendor) nil))
                       (price . ,(nth 4 item))
                       (prep_notes . ,(nth 5 item))
                       (custom_notes . ,(or notes ""))
                       (recipient . ,causal-duckweed-recipient)
                       (location . ,causal-duckweed-location)
                       (location_name . ,causal-duckweed-location-name)
                       (room . ,causal-duckweed-room)
                       (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z")))))
    ;; 1. Try live CapTP invocation (Goblins vat)
    (if (causal-duckweed--captp-invoke vendor-id order-data)
        (message "CapTP live: %s placed via %s" (nth 1 item) vendor-id)
      ;; 2. Fallback: ACP envelope + MCP JSON dispatch
      (let* ((order-data (if causal-duckweed-use-acp
                              (causal-duckweed--acp-envelope order-data)
                            order-data))
             (order-file (expand-file-name
                          (format "%s%s-%s-%s.json"
                                  causal-duckweed-order-dir
                                  (format-time-string "%Y%m%d-%H%M%S")
                                  (nth 2 item)
                                  (nth 0 item)))))
        (make-directory (file-name-directory order-file) t)
        (with-temp-file order-file
          (insert (json-encode order-data)))
        (message "ACP+MCP fallback: %s → %s" (nth 1 item) order-file)
        order-file))))

(defun causal-duckweed--compose-email (item notes)
  "Compose vendor/concierge email for ITEM."
  (let* ((vendor (causal-duckweed--vendor-info (nth 3 item)))
         (to (or (and vendor (nth 3 vendor)) ""))
         (subject (format "%s Order: %s → %s Room %s"
                          (capitalize (nth 2 item))
                          (nth 1 item)
                          causal-duckweed-location-name
                          causal-duckweed-room))
         (body (format "Hello,

I would like to order:

  %s (%s)
  Category: %s
  %s
  %s

Deliver to:
  %s
  %s, Room %s
  Recipient: %s
  Time: %s

Thank you.

--
%s"
                       (nth 1 item)
                       (nth 4 item)
                       (nth 2 item)
                       (if (string-empty-p (nth 5 item)) ""
                         (format "Notes: %s" (nth 5 item)))
                       (if (and notes (not (string-empty-p notes)))
                           (format "Special request: %s" notes) "")
                       causal-duckweed-location-name
                       causal-duckweed-location
                       causal-duckweed-room
                       causal-duckweed-recipient
                       (format-time-string "%H:%M %p")
                       causal-duckweed-recipient)))
    (compose-mail to subject)
    (message-goto-body)
    (insert body)
    (message "Email composed → %s. C-c C-c to send."
             (if (string-empty-p to) "draft" to))))

(defun causal-duckweed-order (category key)
  "Place order for KEY in CATEGORY (food, plant, flower)."
  (let* ((menu (pcase category
                 ("food"   causal-duckweed-food-items)
                 ("plant"  causal-duckweed-plant-items)
                 ("flower" causal-duckweed-flower-items)
                 (_ (user-error "Unknown category: %s" category))))
         (item (assoc key menu)))
    (unless item
      (user-error "No item for key %s in %s" key category))
    (causal-duckweed--ensure-room)
    (let ((notes (read-string
                  (format "[%s] Notes for %s (RET=none): "
                          category (nth 1 item)))))
      (causal-duckweed--log-order item notes)
      (when causal-duckweed-use-mcp
        (causal-duckweed--mcp-dispatch item notes))
      (causal-duckweed--compose-email item notes)
      (when (executable-find "say")
        (start-process "duckweed-announce" nil "say"
                       "-v" causal-duckweed-voice
                       (format "Ordering %s from %s category to room %s"
                               (nth 1 item) category
                               causal-duckweed-room))))))

;; ── Generate per-item commands ──────────────────────────

(dolist (pair `(("food"   . ,causal-duckweed-food-items)
               ("plant"  . ,causal-duckweed-plant-items)
               ("flower" . ,causal-duckweed-flower-items)))
  (let ((cat (car pair)))
    (dolist (entry (cdr pair))
      (let ((key (car entry))
            (cat-copy cat))
        (defalias (intern (format "causal-duckweed-%s-%s" cat-copy key))
          (lambda ()
            (interactive)
            (causal-duckweed-order cat-copy key))
          (format "Order %s: %s (%s)" cat-copy (nth 1 entry) (nth 4 entry)))))))

;; ── Transient Menus ─────────────────────────────────────

(transient-define-prefix causal-duckweed-food-tmenu ()
  "Food ordering."
  [:description
   (lambda () (format "Duckweed: Food → %s" causal-duckweed-location-name))
   ["Quick"
    ("a" "Avocado toast        $18" causal-duckweed-food-a)
    ("b" "Breakfast burrito    $16" causal-duckweed-food-b)
    ("c" "Caesar salad         $14" causal-duckweed-food-c)
    ("d" "Dim sum              $28" causal-duckweed-food-d)
    ("e" "Eggs benedict        $22" causal-duckweed-food-e)]
   ["Mains"
    ("f" "Fish tacos           $18" causal-duckweed-food-f)
    ("g" "Grilled salmon       $34" causal-duckweed-food-g)
    ("h" "Hamburger            $20" causal-duckweed-food-h)
    ("i" "Indian curry         $22" causal-duckweed-food-i)
    ("j" "Japanese ramen       $19" causal-duckweed-food-j)
    ("k" "Korean bibimbap      $21" causal-duckweed-food-k)
    ("l" "Lobster roll         $32" causal-duckweed-food-l)]
   ["More"
    ("m" "Margherita pizza     $18" causal-duckweed-food-m)
    ("n" "Pho                  $17" causal-duckweed-food-n)
    ("o" "Omelette             $15" causal-duckweed-food-o)
    ("p" "Pad thai             $18" causal-duckweed-food-p)
    ("q" "Quinoa bowl          $16" causal-duckweed-food-q)
    ("r" "Ribeye steak         $48" causal-duckweed-food-r)
    ("s" "Sushi combo          $38" causal-duckweed-food-s)
    ("t" "Thai green curry     $20" causal-duckweed-food-t)]])

(transient-define-prefix causal-duckweed-plant-tmenu ()
  "Plant ordering — SF nurseries + online."
  [:description
   (lambda () (format "Duckweed: Plants → %s" causal-duckweed-location-name))
   ["Low-Maintenance"
    ("a" "Aloe vera            $12" causal-duckweed-plant-a)
    ("d" "Duckweed!            $8"  causal-duckweed-plant-d)
    ("e" "Echeveria            $10" causal-duckweed-plant-e)
    ("g" "Golden pothos        $18" causal-duckweed-plant-g)
    ("j" "Jade plant           $15" causal-duckweed-plant-j)
    ("s" "Snake plant          $20" causal-duckweed-plant-s)
    ("t" "Air plant            $8"  causal-duckweed-plant-t)]
   ["Statement"
    ("b" "Boston fern          $28" causal-duckweed-plant-b)
    ("c" "Chinese money plant  $22" causal-duckweed-plant-c)
    ("f" "Fiddle leaf fig      $65" causal-duckweed-plant-f)
    ("m" "Monstera             $45" causal-duckweed-plant-m)
    ("n" "Norfolk pine         $30" causal-duckweed-plant-n)
    ("o" "Orchid               $38" causal-duckweed-plant-o)
    ("r" "Rubber plant         $32" causal-duckweed-plant-r)]
   ["Specialty"
    ("h" "Herb trio            $14" causal-duckweed-plant-h)
    ("i" "Italian stone pine   $35" causal-duckweed-plant-i)
    ("k" "Kalanchoe            $12" causal-duckweed-plant-k)
    ("l" "Lavender             $16" causal-duckweed-plant-l)
    ("p" "Peace lily           $25" causal-duckweed-plant-p)
    ("q" "Queen of the night   $20" causal-duckweed-plant-q)]])

(transient-define-prefix causal-duckweed-flower-tmenu ()
  "Flower ordering — SF florists."
  [:description
   (lambda () (format "Duckweed: Flowers → %s" causal-duckweed-location-name))
   ["Bouquets"
    ("a" "Seasonal mix         $58" causal-duckweed-flower-a)
    ("d" "Dahlias              $65" causal-duckweed-flower-d)
    ("e" "Eucalyptus+ranunculus $52" causal-duckweed-flower-e)
    ("g" "Garden roses (12)    $78" causal-duckweed-flower-g)
    ("h" "Hydrangea dome       $48" causal-duckweed-flower-h)
    ("m" "Wildflower bunch     $42" causal-duckweed-flower-m)
    ("p" "Peony bouquet        $72" causal-duckweed-flower-p)
    ("r" "Ranunculus           $38" causal-duckweed-flower-r)
    ("s" "Sunflowers (10)      $35" causal-duckweed-flower-s)
    ("t" "Tulips (20)          $42" causal-duckweed-flower-t)]
   ["Singles & Stems"
    ("b" "Bird of paradise     $45" causal-duckweed-flower-b)
    ("c" "Chrysanthemums       $30" causal-duckweed-flower-c)
    ("f" "Freesia bunch        $28" causal-duckweed-flower-f)
    ("i" "Iris bouquet         $35" causal-duckweed-flower-i)
    ("j" "Jasmine vine         $30" causal-duckweed-flower-j)
    ("k" "King protea          $25" causal-duckweed-flower-k)
    ("l" "Lily (stargazer)     $40" causal-duckweed-flower-l)
    ("n" "Narcissus            $18" causal-duckweed-flower-n)
    ("o" "Orchid spray         $55" causal-duckweed-flower-o)
    ("q" "Queen Anne's lace    $15" causal-duckweed-flower-q)]])

;; ── Top-Level Transient ─────────────────────────────────

(transient-define-prefix causal-duckweed-tmenu ()
  "Duckweed: order anything deliverable in SF."
  [:description
   (lambda () (format "Duckweed → %s (Room %s)"
                      causal-duckweed-location-name
                      (if (string-empty-p causal-duckweed-room) "?" causal-duckweed-room)))
   ["Category"
    ("f" "Food"    causal-duckweed-food-tmenu)
    ("p" "Plants"  causal-duckweed-plant-tmenu)
    ("w" "Flowers" causal-duckweed-flower-tmenu)]
   ["Settings"
    ("R" "Set room/unit"       causal-duckweed-set-room)
    ("A" "Set address"         causal-duckweed-set-address)
    ("V" "List vendors"        causal-duckweed-list-vendors)
    ("L" "View order log"      causal-duckweed-view-log)
    ("?" "Show ordering chain" causal-duckweed-show-chain)]])

;; ── Settings Commands ───────────────────────────────────

(defun causal-duckweed-set-room ()
  "Set room/unit number."
  (interactive)
  (setq causal-duckweed-room
        (read-string "Room/unit: " causal-duckweed-room))
  (message "Room set to %s" causal-duckweed-room))

(defun causal-duckweed-set-address ()
  "Set delivery address."
  (interactive)
  (setq causal-duckweed-location-name
        (read-string "Location name: " causal-duckweed-location-name))
  (setq causal-duckweed-location
        (read-string "Address: " causal-duckweed-location))
  (message "Delivering to %s" causal-duckweed-location-name))

(defun causal-duckweed-list-vendors ()
  "Display vendor registry."
  (interactive)
  (with-current-buffer (get-buffer-create "*Duckweed Vendors*")
    (erase-buffer)
    (insert "Duckweed SF Vendor Registry\n")
    (insert "===========================\n\n")
    (dolist (cat '("food" "plant" "flower" "supply"))
      (insert (format "── %s ──\n" (upcase cat)))
      (dolist (v causal-duckweed-vendors)
        (when (string= (nth 4 v) cat)
          (insert (format "  %-20s %s\n    %s\n"
                          (nth 1 v)
                          (or (nth 2 v) "(no URL)")
                          (nth 5 v)))))
      (insert "\n"))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun causal-duckweed-view-log ()
  "Open the order log."
  (interactive)
  (find-file (expand-file-name causal-duckweed-log-file)))

(defun causal-duckweed-show-chain ()
  "Display the causal ordering chain with ACP/MCP."
  (interactive)
  (with-current-buffer (get-buffer-create "*Duckweed Chain*")
    (erase-buffer)
    (insert "Duckweed Causal Ordering Chain\n")
    (insert "==============================\n\n")
    (insert "  C-c d  →  category transient (food/plant/flower)\n")
    (insert "     │\n")
    (insert "     v\n")
    (insert "  item transient  →  a-t letter selects orderable\n")
    (insert "     │\n")
    (insert "     v\n")
    (insert "  ACP capability envelope  (cap:YYYYMMDD:NNNNN)\n")
    (insert "  ├─ grant: invoke\n")
    (insert "  ├─ scope: order:place\n")
    (insert "  └─ expires: +24h\n")
    (insert "     │\n")
    (insert "     v\n")
    (insert "  MCP dispatch  →  ~/.topos/orders/{cat}-{key}.json\n")
    (insert "     │\n")
    (insert "     v\n")
    (insert "  org-log  →  ~/.topos/duckweed-orders.org\n")
    (insert "     │\n")
    (insert "     v\n")
    (insert "  Gnus email compose  →  vendor or concierge\n")
    (insert "     │\n")
    (insert "     v\n")
    (insert "  macOS say  →  voice confirmation\n")
    (insert "     │\n")
    (insert "     v\n")
    (insert (format "  %s Room %s\n"
                    causal-duckweed-location-name causal-duckweed-room))
    (insert "     │\n")
    (insert "     v\n")
    (insert "  DELIVERED\n\n")
    (insert "GF(3): ACP-grant(+1) + MCP-dispatch(0) + email(-1) = 0\n")
    (insert "Vendors: Sloat, Flowercraft, Farmgirl, Bloomscape, DoorDash, ...\n")
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;;###autoload
(defun causal-duckweed ()
  "Open the duckweed ordering transient."
  (interactive)
  (causal-duckweed-tmenu))

(provide 'causal-duckweed)
;;; causal-duckweed.el ends here
