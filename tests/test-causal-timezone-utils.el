;;; test-causal-timezone-utils.el --- Causal Make Utils Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'ert)
(require 'causal-timezone-test-utils)
(require 'causal-timezone-utils)

(ert-deftest test-causal-timezone-unicode-get ()
  (let ((causal-lib-use-unicode nil))
    (should (string-equal (causal-timezone-unicode-get :forward) "Forward"))
    (should (string-equal (causal-timezone-unicode-get :backward) "Backward"))
    (should (string-equal (causal-timezone-unicode-get :right) "Right"))
    (should (string-equal (causal-timezone-unicode-get :left) "Left"))
    (should (string-equal (causal-timezone-unicode-get :current) "Current Hour"))
    (should (string-equal (causal-timezone-unicode-get :previous) "Previous"))
    (should (string-equal (causal-timezone-unicode-get :next) "Next")))

  (let ((causal-lib-use-unicode t))
    (should (string-equal (causal-timezone-unicode-get :forward) "→"))
    (should (string-equal (causal-timezone-unicode-get :backward) "←"))
    (should (string-equal (causal-timezone-unicode-get :right) "→"))
    (should (string-equal (causal-timezone-unicode-get :left) "←"))
    (should (string-equal (causal-timezone-unicode-get :current) "⨀"))
    (should (string-equal (causal-timezone-unicode-get :previous) "↑"))
    (should (string-equal (causal-timezone-unicode-get :next) "↓"))))

(ert-deftest test-causal-timezone-planner-tmenu ()
  (let ()
    (causalt-timezone-setup)
    (cl-letf ((causalt-mock #'causal-timezone-jump-to-relative-now)
              (causalt-mock #'previous-line)
              (causalt-mock #'next-line)
              (causalt-mock #'causal-timezone-planner-forward-day)
              (causalt-mock #'causal-timezone-planner-backward-day)
              (causalt-mock #'causal-timezone-planner-current-time)
              (causalt-mock #'causal-timezone-planner-current-local)
              (causalt-mock #'causal-timezone-planner-current-point)
              (causalt-mock #'vtable-next-column)
              (causalt-mock #'vtable-previous-column)
              (causalt-mock #'causal-timezone-planner)
              (causalt-mock #'world-clock)
              (causalt-mock #'quit-window))

      (let ((test-vectors
             '((:binding "." :command causal-timezone-jump-to-relative-now)
               (:binding "n" :command next-line)
               (:binding "p" :command previous-line)
               (:binding "f" :command causal-timezone-planner-forward-day)
               (:binding "b" :command causal-timezone-planner-backward-day)
               (:binding "t" :command causal-timezone-planner-current-time)
               (:binding "l" :command causal-timezone-planner-current-local)
               (:binding "TAB" :command vtable-next-column)
               (:binding "S-TAB" :command vtable-previous-column)
               (:binding "T" :command causal-timezone-planner-current-point)
               (:binding "z" :command causal-timezone-planner)
               (:binding "w" :command world-clock)
               (:binding "q" :command quit-window))))

        (causalt-suffix-testcase-runner test-vectors
                                        #'causal-timezone-planner-tmenu
                                        '(lambda () (random 5000)))))
    (causalt-timezone-breakdown)))

(ert-deftest test-causal-timezone--date-formatter ()
  (let* ((now (list 26671 48225 355484 0))
         (causal-timezone-datestamp-format "%a %b %-e %Y, %l:%M %p")
         (control "Thu May 22 2025,  5:08 PM ☼")
         (result (causal-timezone--date-formatter now)))

    (should (string-equal control result))))

(ert-deftest test-causal-timezone-zone-info ()
  ;; !!!: This count of zones for macOS only. Subject to change.
  (let* ((control 598)
         (zone-info (causal-timezone-zone-info))
         (result (length zone-info)))
    (should (= control result))))

;; !!!: is this function being used?
(ert-deftest test-causal-timezone-map-local-to-timezone ()
  (let* ((ts "2025-05-23")
         (remote-tz "Europe/Berlin")
         (control "2025-05-23 10:00:00 CEST")
         (result (causal-timezone-map-local-to-timezone ts remote-tz)))
    (should (string-equal control result))))

(ert-deftest test-causal-timezone-offset-8601 ()
  ;; !!!: This test will only work in Pacific Time.
  (let* ((control "+0200")
         (offset 7200)
         (result (causal-timezone-offset-8601 offset)))
    (should (string-equal control result))))


(ert-deftest test-causal-timezone-planner-mode-map ()
  (let ((test-map causal-timezone-planner-mode-map))
    (should (eq (keymap-lookup test-map "C-o") #'causal-timezone-planner-tmenu))
    (should (eq (keymap-lookup test-map ".") #'causal-timezone-jump-to-relative-now))
    (should (eq (keymap-lookup test-map "t") #'causal-timezone-planner-current-time))
    (should (eq (keymap-lookup test-map "l") #'causal-timezone-planner-current-local))
    (should (eq (keymap-lookup test-map "T") #'causal-timezone-planner-current-point))
    (should (eq (keymap-lookup test-map "p") #'previous-line))
    (should (eq (keymap-lookup test-map "n") #'next-line))
    (should (eq (keymap-lookup test-map "q") #'quit-window))
    (should (eq (keymap-lookup test-map "j") #'next-line))
    (should (eq (keymap-lookup test-map "k") #'previous-line))
    (should (eq (keymap-lookup test-map "w") #'world-clock))
    (should (eq (keymap-lookup test-map "z") #'causal-timezone-planner))
    (should (eq (keymap-lookup test-map "c") #'calendar))))

(ert-deftest test-causal-timezone-local-time-to-remote ()
  (let* ((read-date "2025-05-23 12:00")
         (remote-tz "Europe/Berlin")
         (control "Europe/Berlin 2025-05-23 22:00:00 CEST")
         (result (causal-timezone-local-time-to-remote read-date remote-tz)))

    (should (string-equal control result))))

(ert-deftest test-causal-timezone-local-time-to-remote-victoria ()
  (let* ((read-date "2025-05-23 12:00")
         (remote-tz "Australia/Victoria")
         (control "Australia/Victoria 2025-05-24 06:00:00 AEST")
         (result (causal-timezone-local-time-to-remote read-date remote-tz)))

    (should (string-equal control result))))

(defun in-daylight-saving-time-p ()
  "Return t if the current local time is in Daylight Saving Time (DST), nil otherwise."
  (let ((dst (nth 8 (decode-time (current-time)))))
    (and dst (not (eq dst 0)))))

(ert-deftest test-causal-timezone-remote-time-to-local ()
  (let* ((read-date (if (in-daylight-saving-time-p)
                        "2025-05-23 11:00"
                      "2025-05-23 12:00"))
         (remote-tz "Europe/Berlin")
         (control "2025-05-23 03:00:00 PDT")
         (result (causal-timezone-remote-time-to-local read-date remote-tz)))

    (should (string-equal control result))))

(ert-deftest test-timezone-sanity-check ()
  (let* ((ts "2025-06-04T17:00")
         (control "2025-06-04 17:00:00 AEST")
         (result
          (format-time-string
           causal-timezone-convert-datestamp-format
           (encode-time (iso8601-parse (concat ts "+1000")))
           "Australia/Victoria")))

    (should (string-equal control result))))

;; (format-time-string causal-timezone-convert-datestamp-format (encode-time (iso8601-parse "2025-06-04T17:00:00+0200")) "America/New_York")
;; 2025-06-04 11:00:00 EDT

(ert-deftest test-timezone-sanity-check2 ()
  (let ((ts "2025-06-04T17:00")
        (db (list
             (list "+1000" "Australia/Victoria" "2025-06-04 17:00:00 AEST")
             (list "+0200" "Europe/Berlin" "2025-06-04 17:00:00 CEST")
             (list "+0500" "Asia/Karachi" "2025-06-04 17:00:00 PKT")
             (list "+0530" "Asia/Kolkata" "2025-06-04 17:00:00 IST")
             (list "+0900" "Asia/Seoul" "2025-06-04 17:00:00 KST")
             (list "-0700" "America/Los_Angeles" "2025-06-04 17:00:00 PDT")
             (list "-0400" "America/New_York" "2025-06-04 17:00:00 EDT"))))

    (mapc (lambda (test-vector)
            (let* ((offset (nth 0 test-vector))
                   (tz (nth 1 test-vector))
                   (control (nth 2 test-vector))
                   (result
                    (format-time-string
                     causal-timezone-convert-datestamp-format
                     (encode-time (iso8601-parse (concat ts offset)))
                     tz)))
              (print control)
              (should (string-equal result control))))
          db)))


(ert-deftest test-causal-timezone--zone-seconds-to-hours ()
  ;; (current-time-zone (current-time) "America/Chicago")
  (should (string-equal (causal-timezone--zone-seconds-to-hours 32400) "+0900"))
  (should (string-equal (causal-timezone--zone-seconds-to-hours 36000) "+1000"))
  (should (string-equal (causal-timezone--zone-seconds-to-hours -18000) "-0500"))
  (should (string-equal (causal-timezone--zone-seconds-to-hours +28800) "+0800"))
  )

(provide 'test-causal-timezone-utils)
;;; test-causal-timezone-utils.el ends here
