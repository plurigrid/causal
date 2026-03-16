;;; causal-food.el --- Transient food ordering for InterContinental SF -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Plurigrid
;; Author: Barton Rhodes <barton@plurigrid.com>
;; Keywords: convenience, transient, food, mcp
;; Requires: transient, message

;;; Commentary:
;;
;; Causally closed food ordering chain via Emacs transient.
;; Press a letter → select food → compose order → send to concierge.
;;
;; Chain: keystroke → transient → org-log → email/MCP → concierge → food
;;
;; Bound globally to C-c f (for food).

;;; Code:

(require 'transient)
(require 'message)

(defgroup causal-food nil
  "Food ordering via Emacs transient."
  :group 'convenience
  :prefix "causal-food-")

(defcustom causal-food-hotel "InterContinental San Francisco"
  "Hotel name for orders."
  :type 'string)

(defcustom causal-food-address "888 Howard Street, San Francisco, CA 94103"
  "Hotel address."
  :type 'string)

(defcustom causal-food-room ""
  "Room number (set per-session)."
  :type 'string)

(defcustom causal-food-guest-name "Barton Rhodes"
  "Guest name for orders."
  :type 'string)

(defcustom causal-food-concierge-email nil
  "Concierge email. If nil, composes draft only."
  :type '(choice (const nil) string))

(defcustom causal-food-log-file "~/.topos/food-orders.org"
  "Org file for order logging."
  :type 'string)

(defcustom causal-food-use-mcp t
  "If non-nil, attempt MCP dispatch before email fallback."
  :type 'boolean)

;; ── Menu Items ──────────────────────────────────────────
;; Each item: (key label category price-estimate prep-notes)

(defvar causal-food-menu
  '(;; Quick bites (a-f)
    ("a" "Avocado toast + coffee"           "breakfast"  "$18" "")
    ("b" "Breakfast burrito"                 "breakfast"  "$16" "scrambled eggs, beans, salsa")
    ("c" "Caesar salad"                      "lunch"      "$14" "add chicken +$6")
    ("d" "Dim sum platter"                   "lunch"      "$28" "for 2, steamed + fried mix")
    ("e" "Eggs benedict"                     "breakfast"  "$22" "hollandaise on the side ok")
    ("f" "Fish tacos"                        "lunch"      "$18" "baja style, lime crema")

    ;; Mains (g-l)
    ("g" "Grilled salmon"                    "dinner"     "$34" "with roasted vegetables")
    ("h" "Hamburger + fries"                 "lunch"      "$20" "medium rare, brioche bun")
    ("i" "Indian curry (tikka masala)"       "dinner"     "$22" "with naan and rice")
    ("j" "Japanese ramen (tonkotsu)"         "dinner"     "$19" "extra chashu +$4")
    ("k" "Korean bibimbap"                   "dinner"     "$21" "stone pot, extra spicy ok")
    ("l" "Lobster roll"                      "lunch"      "$32" "Connecticut style, warm butter")

    ;; Pizza & pasta (m-p)
    ("m" "Margherita pizza"                  "dinner"     "$18" "wood-fired if available")
    ("n" "Noodle soup (pho)"                 "dinner"     "$17" "rare beef, extra herbs")
    ("o" "Omelette (custom)"                 "breakfast"  "$15" "specify fillings at prompt")
    ("p" "Pad thai"                          "dinner"     "$18" "shrimp, medium spice")

    ;; Specials (q-t)
    ("q" "Quinoa bowl"                       "lunch"      "$16" "vegan, roasted veg")
    ("r" "Ribeye steak"                      "dinner"     "$48" "12oz, medium rare")
    ("s" "Sushi combo (chef's choice)"       "dinner"     "$38" "12pc nigiri + roll")
    ("t" "Thai green curry"                  "dinner"     "$20" "with jasmine rice")

    ;; Drinks & dessert (u-z)
    ("u" "Udon (tempura)"                    "dinner"     "$17" "hot broth")
    ("v" "Vietnamese banh mi"                "lunch"      "$14" "lemongrass chicken")
    ("w" "Wine + cheese plate"               "appetizer"  "$28" "house selection")
    ("x" "Xiao long bao (soup dumplings)"   "appetizer"  "$16" "8pc, ginger-vinegar dip")
    ("y" "Yogurt parfait + granola"          "breakfast"  "$12" "add berries")
    ("z" "Za'atar flatbread + hummus"        "appetizer"  "$14" "warm pita"))
  "A-Z food menu. Each entry: (KEY LABEL CATEGORY PRICE NOTES).")

;; ── Core Functions ──────────────────────────────────────

(defun causal-food--ensure-room ()
  "Ensure room number is set."
  (when (string-empty-p causal-food-room)
    (setq causal-food-room
          (read-string "Room number: ")))
  causal-food-room)

(defun causal-food--log-order (item notes)
  "Log order to org file. ITEM is menu entry, NOTES is custom notes."
  (let ((file (expand-file-name causal-food-log-file)))
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* ORDER %s [%s]\n"
                      (nth 1 item)
                      (format-time-string "%Y-%m-%d %H:%M")))
      (insert (format ":PROPERTIES:\n"))
      (insert (format ":HOTEL: %s\n" causal-food-hotel))
      (insert (format ":ROOM: %s\n" causal-food-room))
      (insert (format ":CATEGORY: %s\n" (nth 2 item)))
      (insert (format ":PRICE: %s\n" (nth 3 item)))
      (insert (format ":NOTES: %s\n" (or notes "")))
      (insert (format ":ORDERED: %s\n" (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
      (insert (format ":END:\n"))
      (save-buffer)
      (message "Order logged to %s" file))))

(defun causal-food--compose-email (item notes)
  "Compose email to concierge for ITEM with NOTES."
  (let ((to (or causal-food-concierge-email ""))
        (subject (format "Room %s - Food Order: %s"
                         causal-food-room (nth 1 item)))
        (body (format "Hello,

I would like to order the following to room %s:

  %s (%s, %s)
  %s
  %s

Guest: %s
Room: %s
Time: %s

Thank you.

--
%s"
                      causal-food-room
                      (nth 1 item)
                      (nth 2 item)
                      (nth 3 item)
                      (if (string-empty-p (nth 4 item)) "" (format "Notes: %s" (nth 4 item)))
                      (if (and notes (not (string-empty-p notes)))
                          (format "Special request: %s" notes) "")
                      causal-food-guest-name
                      causal-food-room
                      (format-time-string "%H:%M %p")
                      causal-food-guest-name)))
    (compose-mail to subject)
    (message-goto-body)
    (insert body)
    (message "Order email composed. C-c C-c to send.")))

(defun causal-food--mcp-dispatch (item notes)
  "Dispatch order via MCP shell command. ITEM with NOTES."
  (let* ((order-json (json-encode
                      `((hotel . ,causal-food-hotel)
                        (room . ,causal-food-room)
                        (guest . ,causal-food-guest-name)
                        (item . ,(nth 1 item))
                        (category . ,(nth 2 item))
                        (price . ,(nth 3 item))
                        (notes . ,(or notes ""))
                        (prep_notes . ,(nth 4 item))
                        (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z")))))
         (order-file (expand-file-name
                      (format "~/.topos/orders/%s-%s.json"
                              (format-time-string "%Y%m%d-%H%M%S")
                              (nth 0 item)))))
    ;; Write order JSON
    (make-directory (file-name-directory order-file) t)
    (with-temp-file order-file
      (insert order-json))
    (message "MCP order dispatched: %s → %s" (nth 1 item) order-file)
    order-file))

(defun causal-food-order (key)
  "Place order for item at KEY."
  (let ((item (assoc key causal-food-menu)))
    (unless item
      (user-error "No menu item for key: %s" key))
    (causal-food--ensure-room)
    (let ((notes (read-string
                  (format "Special notes for %s (RET for none): "
                          (nth 1 item)))))
      ;; Log to org
      (causal-food--log-order item notes)
      ;; MCP dispatch (writes JSON)
      (when causal-food-use-mcp
        (causal-food--mcp-dispatch item notes))
      ;; Compose email
      (causal-food--compose-email item notes)
      ;; Announce
      (when (executable-find "say")
        (start-process "food-announce" nil "say" "-v" "Samantha"
                       (format "Ordering %s to room %s"
                               (nth 1 item) causal-food-room))))))

;; ── Generate order commands for each letter ─────────────

(dolist (entry causal-food-menu)
  (let ((key (car entry)))
    (defalias (intern (format "causal-food-order-%s" key))
      (lambda ()
        (interactive)
        (causal-food-order key))
      (format "Order: %s (%s)" (nth 1 entry) (nth 3 entry)))))

;; ── Transient Menu ──────────────────────────────────────

(transient-define-prefix causal-food-tmenu ()
  "Food ordering transient for InterContinental SF."
  [:description
   (lambda () (format "Order Food → %s (Room %s)"
                      causal-food-hotel
                      (if (string-empty-p causal-food-room) "?" causal-food-room)))
   ["Breakfast"
    ("a" "Avocado toast + coffee     $18" causal-food-order-a)
    ("b" "Breakfast burrito          $16" causal-food-order-b)
    ("e" "Eggs benedict              $22" causal-food-order-e)
    ("o" "Omelette (custom)          $15" causal-food-order-o)
    ("y" "Yogurt parfait + granola   $12" causal-food-order-y)]
   ["Lunch"
    ("c" "Caesar salad               $14" causal-food-order-c)
    ("d" "Dim sum platter            $28" causal-food-order-d)
    ("f" "Fish tacos                 $18" causal-food-order-f)
    ("h" "Hamburger + fries          $20" causal-food-order-h)
    ("l" "Lobster roll               $32" causal-food-order-l)
    ("q" "Quinoa bowl                $16" causal-food-order-q)
    ("v" "Vietnamese banh mi         $14" causal-food-order-v)]
   ["Dinner"
    ("g" "Grilled salmon             $34" causal-food-order-g)
    ("i" "Indian curry               $22" causal-food-order-i)
    ("j" "Japanese ramen             $19" causal-food-order-j)
    ("k" "Korean bibimbap            $21" causal-food-order-k)
    ("m" "Margherita pizza           $18" causal-food-order-m)
    ("n" "Noodle soup (pho)          $17" causal-food-order-n)
    ("p" "Pad thai                   $18" causal-food-order-p)
    ("r" "Ribeye steak               $48" causal-food-order-r)
    ("s" "Sushi combo                $38" causal-food-order-s)
    ("t" "Thai green curry           $20" causal-food-order-t)
    ("u" "Udon (tempura)             $17" causal-food-order-u)]
   ["Appetizer/Drinks"
    ("w" "Wine + cheese plate        $28" causal-food-order-w)
    ("x" "Xiao long bao              $16" causal-food-order-x)
    ("z" "Za'atar flatbread          $14" causal-food-order-z)]
   ["Actions"
    ("R" "Set room number" causal-food-set-room)
    ("L" "View order log" causal-food-view-log)
    ("?" "Show order chain" causal-food-show-chain)]])

(defun causal-food-set-room ()
  "Interactively set room number."
  (interactive)
  (setq causal-food-room (read-string "Room number: " causal-food-room))
  (message "Room set to %s" causal-food-room))

(defun causal-food-view-log ()
  "Open the food order log."
  (interactive)
  (find-file (expand-file-name causal-food-log-file)))

(defun causal-food-show-chain ()
  "Display the causal ordering chain."
  (interactive)
  (with-current-buffer (get-buffer-create "*Food Order Chain*")
    (erase-buffer)
    (insert "Causal Food Order Chain\n")
    (insert "=======================\n\n")
    (insert "  keystroke (a-z)\n")
    (insert "      |\n")
    (insert "      v\n")
    (insert "  transient-menu (causal-food-tmenu)\n")
    (insert "      |\n")
    (insert "      v\n")
    (insert "  org-log (~/.topos/food-orders.org)\n")
    (insert "      |\n")
    (insert "      v\n")
    (insert "  MCP dispatch (~/.topos/orders/*.json)  -- ACP capability grant\n")
    (insert "      |\n")
    (insert "      v\n")
    (insert "  email compose (Gnus → concierge)\n")
    (insert "      |\n")
    (insert "      v\n")
    (insert "  say announce (macOS TTS confirmation)\n")
    (insert "      |\n")
    (insert "      v\n")
    (insert (format "  %s Room %s\n" causal-food-hotel causal-food-room))
    (insert "      |\n")
    (insert "      v\n")
    (insert "  FOOD ARRIVES\n\n")
    (insert "GF(3) conservation: log(0) + dispatch(+1) + email(-1) = 0\n")
    (display-buffer (current-buffer))))

;;;###autoload
(defun causal-food ()
  "Open the food ordering transient."
  (interactive)
  (causal-food-tmenu))

(provide 'causal-food)
;;; causal-food.el ends here
