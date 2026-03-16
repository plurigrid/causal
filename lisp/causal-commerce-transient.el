;;; causal-commerce-transient.el --- Causal Transient for commerce item dispatch -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Plurigrid Contributors
;; Author: Droid W (seed 0xBF61927E946C4DC1, trit ERGODIC)
;; Keywords: tools, applications
;; URL: https://github.com/plurigrid/causal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Causally closed chain for item commerce:
;;   Emacs keypress -> transient dispatch -> commerce-dispatch.bb -> service API/draft -> item arrives
;;
;; GF(3) trit coloring of commerce channels:
;;   MINUS (-1) = online/return-based (Zappos) -- high optionality, slow
;;   ERGODIC (0) = hybrid (Instacart, Nordstrom, DoorDash) -- balanced
;;   PLUS (+1)  = physical proxy (TaskRabbit) -- haptic, fast, costly
;;
;; OLC tile grid: items sourced from stores at known OLC coordinates;
;;   the transient header shows a live tile readout so you know which
;;   delivery zone you are dispatching into.
;;
;; TERY: Trit / Entropy (gay seed) / Role (commerce channel) / Yoneda (delivery window)

;;; Code:

(require 'transient)
(require 'json)

;;; --- OLC Tile Display ---

(defvar commerce--olc-user-tile "849VQHFJ+X6"
  "User OLC tile (SF default from sf_store_olc_tiles.json).")

(defvar commerce--olc-store-tiles
  '(("Instacart"  . "849VQHPW+H6")
    ("Zappos"     . "85865V63+V2")
    ("Nordstrom"  . "849VQHMV+H2")
    ("TaskRabbit" . "849VQHMR+X6")
    ("DoorDash"   . "849VQHFJ+X6")
    ("Adafruit"   . "87G8Q2JM+2V")
    ("DigiKey"    . "86P8WR55+PX")
    ("Amazon"     . "849VQHFJ+X6"))
  "OLC tiles for commerce channel dispatch points.")

(defun commerce--olc-header ()
  "Return a formatted OLC tile header string for the transient."
  (let ((tiles (mapconcat
                (lambda (pair)
                  (format "  %s %s" (car pair) (cdr pair)))
                commerce--olc-store-tiles
                "\n")))
    (format "OLC Dispatch Grid [user tile: %s]\n%s"
            commerce--olc-user-tile tiles)))

(defun commerce--gf3-indicator (trit)
  "Return a GF(3) color indicator string for TRIT (-1, 0, or +1)."
  (pcase trit
    (-1 (propertize "[-1]" 'face '(:foreground "dodger blue")))
    (0  (propertize "[ 0]" 'face '(:foreground "green3")))
    (1  (propertize "[+1]" 'face '(:foreground "orange red")))
    (_  "[??]")))

;;; --- Configuration ---

(defgroup commerce-dispatch nil
  "Item commerce via causal transient dispatch."
  :group 'causal
  :prefix "commerce-dispatch-")

(defcustom commerce-dispatch-city "San Francisco"
  "City for local pickup/delivery."
  :type 'string
  :group 'commerce-dispatch)

(defcustom commerce-dispatch-address ""
  "Delivery address."
  :type 'string
  :group 'commerce-dispatch)

(defcustom commerce-dispatch-budget "$100-250"
  "Budget range for item acquisition."
  :type 'string
  :group 'commerce-dispatch)

(defcustom commerce-dispatch-constraint ""
  "Item constraint description for proxies."
  :type 'string
  :group 'commerce-dispatch)

(defcustom commerce-dispatch-script
  (expand-file-name "commerce-dispatch.bb"
                    (file-name-directory
                     (or load-file-name buffer-file-name "~/worlds/t/")))
  "Path to commerce-dispatch.bb backing script."
  :type 'string
  :group 'commerce-dispatch)

;;; --- State ---

(defvar commerce-dispatch--last nil "Last commerce dispatch.")
(defvar commerce-dispatch--history nil "Dispatch history this session.")

;;; --- Dispatch ---

(defun commerce-dispatch--ensure-config ()
  "Ensure address is set."
  (when (string-empty-p commerce-dispatch-address)
    (setq commerce-dispatch-address
          (read-string "Delivery address: "))))

(defun commerce-dispatch--place (channel item &optional notes)
  "Place item order via CHANNEL for ITEM with optional NOTES."
  (commerce-dispatch--ensure-config)
  (let* ((notes (or notes (read-string
                           (format "Notes for %s (RET for none): " item)
                           nil nil "")))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (olc-tile (or (cdr (assoc channel commerce--olc-store-tiles))
                       commerce--olc-user-tile))
         (order `((channel . ,channel)
                  (item . ,item)
                  (constraint . ,commerce-dispatch-constraint)
                  (budget . ,commerce-dispatch-budget)
                  (city . ,commerce-dispatch-city)
                  (address . ,commerce-dispatch-address)
                  (olc_tile . ,olc-tile)
                  (notes . ,notes)
                  (timestamp . ,timestamp))))
    (setq commerce-dispatch--last order)
    (push order commerce-dispatch--history)
    (let ((order-file (format "/tmp/commerce-dispatch-%s.json"
                              (format-time-string "%s"))))
      (with-temp-buffer
        (insert (json-encode order))
        (write-region (point-min) (point-max) order-file))
      (message "Dispatching %s via %s [OLC %s]..."
               item channel olc-tile)
      (set-process-sentinel
       (start-process "commerce-dispatch" "*commerce-dispatch*"
                      "bb" commerce-dispatch-script
                      "--channel" channel
                      "--item" item
                      "--constraint" commerce-dispatch-constraint
                      "--budget" commerce-dispatch-budget
                      "--city" commerce-dispatch-city
                      "--address" commerce-dispatch-address
                      "--olc-tile" olc-tile
                      "--notes" notes
                      "--order-file" order-file)
       (lambda (proc event)
         (when (string-match-p "finished" event)
           (message "Commerce dispatch complete: %s via %s [OLC %s]"
                    item channel olc-tile)))))))

;;; --- PLUS (+1): Physical proxy -- TaskRabbit ---

(transient-define-suffix commerce-dispatch-taskrabbit ()
  :key "t"
  :description (lambda () (format "%s TaskRabbit -- rent-a-human proxy"
                                  (commerce--gf3-indicator 1)))
  (interactive)
  (let ((item (read-string "Item for TaskRabbit to acquire: ")))
    (commerce-dispatch--place "TaskRabbit" item)))

(transient-define-suffix commerce-dispatch-taskrabbit-quick ()
  :key "T"
  :description (lambda () (format "%s TaskRabbit -- quick errand (custom)"
                                  (commerce--gf3-indicator 1)))
  (interactive)
  (let ((item (read-string "Quick errand description: ")))
    (commerce-dispatch--place "TaskRabbit" item
                              "URGENT: same-day physical proxy dispatch.")))

;;; --- ERGODIC (0): Hybrid -- Instacart / Nordstrom / DoorDash ---

(transient-define-suffix commerce-dispatch-instacart ()
  :key "i"
  :description (lambda () (format "%s Instacart -- store item delivery"
                                  (commerce--gf3-indicator 0)))
  (interactive)
  (let ((item (read-string "Item for Instacart delivery: ")))
    (commerce-dispatch--place "Instacart" item)))

(transient-define-suffix commerce-dispatch-nordstrom ()
  :key "n"
  :description (lambda () (format "%s Nordstrom -- stylist / ship-to-store"
                                  (commerce--gf3-indicator 0)))
  (interactive)
  (let ((item (read-string "Item for Nordstrom (or 'stylist picks'): ")))
    (commerce-dispatch--place "Nordstrom" item
                              (format "Budget: %s. Constraint: %s."
                                      commerce-dispatch-budget
                                      commerce-dispatch-constraint))))

(transient-define-suffix commerce-dispatch-doordash ()
  :key "d"
  :description (lambda () (format "%s DoorDash -- local merchant delivery"
                                  (commerce--gf3-indicator 0)))
  (interactive)
  (let* ((merchant (read-string "DoorDash merchant/store: "))
         (item (read-string (format "Item from %s: " merchant))))
    (commerce-dispatch--place "DoorDash"
                              (format "%s (from %s)" item merchant))))

;;; --- MINUS (-1): Online / return-based -- Zappos ---

(transient-define-suffix commerce-dispatch-zappos ()
  :key "z"
  :description (lambda () (format "%s Zappos -- free returns, 365 days"
                                  (commerce--gf3-indicator -1)))
  (interactive)
  (let ((item (read-string "Item for Zappos: ")))
    (commerce-dispatch--place "Zappos" item
                              "Free shipping + free 365-day returns.")))

(transient-define-suffix commerce-dispatch-zappos-explore ()
  :key "Z"
  :description (lambda () (format "%s Zappos -- browse category"
                                  (commerce--gf3-indicator -1)))
  (interactive)
  (let ((category (completing-read "Zappos category: "
                                   '("footwear" "clothing" "bags" "accessories")
                                   nil t)))
    (commerce-dispatch--place "Zappos"
                              (format "Browse %s -- send top 3 picks" category)
                              "Exploratory: order 2 sizes, return what doesn't fit.")))

;;; --- ELECTRONICS: Adafruit / SparkFun / DigiKey / Amazon ---

(transient-define-suffix commerce-dispatch-electronics ()
  :key "e"
  :description (lambda () (format "%s Electronics — Adafruit/SparkFun/DigiKey"
                                  (commerce--gf3-indicator 0)))
  (interactive)
  (let ((item (read-string "Electronics part (or 'hero-bom' for full BOM): ")))
    (commerce-dispatch--place "Adafruit" item
                              "Route: Adafruit → SparkFun → DigiKey → Amazon fallback.")))

(transient-define-suffix commerce-dispatch-hero-bom ()
  :key "H"
  :description (lambda () (format "%s Hero BOM — full pill dispenser parts list"
                                  (commerce--gf3-indicator 1)))
  (interactive)
  (let ((default-directory (file-name-directory
                            (or load-file-name buffer-file-name "~/worlds/t/"))))
    (message "Dispatching Hero BOM via order-anything-causal.bb hero-order-all...")
    (set-process-sentinel
     (start-process "hero-bom" "*hero-bom-order*"
                    "bb" (expand-file-name "order-anything-causal.bb"
                                           default-directory)
                    "hero-order-all")
     (lambda (proc event)
       (when (string-match-p "finished" event)
         (message "Hero BOM dispatch complete. See /tmp/universal-orders/hero-manifest.json")
         (display-buffer "*hero-bom-order*"))))))

;;; --- Meta ---

(transient-define-suffix commerce-dispatch-history ()
  :key "?"
  :description "Dispatch history"
  (interactive)
  (with-current-buffer (get-buffer-create "*Commerce Dispatches*")
    (erase-buffer)
    (insert "Commerce Dispatch -- Causal Order History\n")
    (insert (make-string 48 ?=) "\n")
    (insert (format "User OLC tile: %s\n\n" commerce--olc-user-tile))
    (if commerce-dispatch--history
        (dolist (order (reverse commerce-dispatch--history))
          (insert (format "[%s] %s -> %s [OLC %s]%s\n"
                          (alist-get 'timestamp order)
                          (alist-get 'channel order)
                          (alist-get 'item order)
                          (alist-get 'olc_tile order)
                          (let ((n (alist-get 'notes order)))
                            (if (string-empty-p n) "" (format " [%s]" n))))))
      (insert "No dispatches yet.\n"))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(transient-define-suffix commerce-dispatch-nash ()
  :key "N"
  :description "Nash equilibrium analysis"
  (interactive)
  (message (concat "Nash: <$50->Zappos(z), $50-150->Instacart(i)/DoorDash(d), "
                   "$150-300->Nordstrom(n), $300+->TaskRabbit(t)")))

(transient-define-suffix commerce-dispatch-olc-grid ()
  :key "O"
  :description "Show OLC tile grid"
  (interactive)
  (with-current-buffer (get-buffer-create "*Commerce OLC Grid*")
    (erase-buffer)
    (insert "Commerce Dispatch -- OLC Tile Grid\n")
    (insert (make-string 40 ?=) "\n\n")
    (insert (format "User tile: %s\n\n" commerce--olc-user-tile))
    (dolist (pair commerce--olc-store-tiles)
      (insert (format "  %-12s  %s\n" (car pair) (cdr pair))))
    (insert "\n")
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;; --- Sub-menus ---

(transient-define-prefix commerce-dispatch-physical ()
  "PLUS (+1): Physical proxy -- rent-a-human"
  [["TaskRabbit / Rent-A-Human"
    (commerce-dispatch-taskrabbit)
    (commerce-dispatch-taskrabbit-quick)]])

(transient-define-prefix commerce-dispatch-hybrid ()
  "ERGODIC (0): Instacart / Nordstrom / DoorDash"
  [["Hybrid Commerce Channels"
    (commerce-dispatch-instacart)
    (commerce-dispatch-nordstrom)
    (commerce-dispatch-doordash)]])

(transient-define-prefix commerce-dispatch-online ()
  "MINUS (-1): Online / return-based"
  [["Online Channels"
    (commerce-dispatch-zappos)
    (commerce-dispatch-zappos-explore)]])

;;; --- Main transient ---

(transient-define-prefix causal-commerce-dispatch ()
  [:description
   (lambda ()
     (concat
      "Commerce Dispatch -- plurigrid/causal\n\n"
      (commerce--olc-header) "\n\n"
      "GF(3) Channel Grid:\n"
      (format "  %s PLUS (+1)  TaskRabbit -- haptic, fast, costly\n"
              (commerce--gf3-indicator 1))
      (format "  %s ERGODIC(0) Instacart/Nordstrom/DoorDash -- balanced\n"
              (commerce--gf3-indicator 0))
      (format "  %s MINUS (-1) Zappos -- free returns, slow\n"
              (commerce--gf3-indicator -1))
      "\n"
      "Nash: <$50->z  $50-150->i/d  $150-300->n  $300+->t"))]
  ["GF(3) Channels"
   ("+" "PLUS: Rent-A-Human (TaskRabbit)>" commerce-dispatch-physical)
   ("0" "ERGODIC: Instacart / Nordstrom / DoorDash>" commerce-dispatch-hybrid)
   ("-" "MINUS: Online / Returns>" commerce-dispatch-online)]
  ["One-Key Commerce"
   ("i" "Instacart item" commerce-dispatch-instacart)
   ("z" "Zappos item (free returns)" commerce-dispatch-zappos)
   ("n" "Nordstrom stylist/ship" commerce-dispatch-nordstrom)
   ("t" "TaskRabbit proxy" commerce-dispatch-taskrabbit)
   ("d" "DoorDash merchant" commerce-dispatch-doordash)
   ("e" "Electronics part" commerce-dispatch-electronics)
   ("H" "Hero BOM (full dispenser)" commerce-dispatch-hero-bom)]
  ["Meta"
   ("N" "Nash equilibrium" commerce-dispatch-nash)
   ("O" "OLC tile grid" commerce-dispatch-olc-grid)
   ("?" "Dispatch history" commerce-dispatch-history)
   ("q" "Quit" transient-quit-all)])

(global-set-key (kbd "C-c C") #'causal-commerce-dispatch)

(provide 'causal-commerce-transient)
;;; causal-commerce-transient.el ends here
