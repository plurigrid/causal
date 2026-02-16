;;; test-causal-calc-financial.el --- Causal Financial Menu Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Y. Choi

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
(require 'cl-lib)
(require 'ert)
(require 'causal-calc-test-utils)
(require 'causal-calc-financial)

;; Functions -----

(ert-deftest test-causal-calc--fin-npv ()
  (causalt-calc-setup)

  (calc-push '(vec 2000 2000 2000 2000))
  (causalt-testbench-calc-fn 'causal-calc--fin-npv
                             '("--rate=9")
                             '(float 647943975411 -8))

  (calc-push '(vec 2000 2000 2000 2000))
  (causalt-testbench-calc-fn 'causal-calc--fin-npv
                             '("--rate=9" "--beginning")
                             '(float 706258933198 -8))

  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--fin-irr ()
  (causalt-calc-setup)

  (calc-push '(vec -2000 100 150 200 250 300 350 400 450 500 550 600))
  (causalt-testbench-calc-fn 'causal-calc--fin-irr
                             '()
                             '(calcFunc-percent
	                       (float 974404201651 -11)))

  (calc-push '(vec -2000 100 150 200 250 300 350 400 450 500 550 600))
  (causalt-testbench-calc-fn 'causal-calc--fin-irr
                             '("--beginning")
                             '(calcFunc-percent
	                       (float 974404201651 -11)))
  ;; TODO: same value, does inverse calc-fin-irr really work?
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--fin-fv-periodic ()
  (causalt-calc-setup)

  (causalt-testbench-calc-fn 'causal-calc--fin-fv-periodic
                             '("--rate=9" "--periods=4" "--amount=2000")
                             '(float 9146258 -3))

  (causalt-testbench-calc-fn 'causal-calc--fin-fv-periodic
                             '("--rate=9" "--periods=4" "--amount=2000" "--beginning")
                             '(float 996942122 -5))

  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--fin-pv-periodic ()
  (causalt-calc-setup)

  (causalt-testbench-calc-fn 'causal-calc--fin-pv-periodic
                             '("--rate=9" "--periods=4" "--amount=2000")
                             '(float 647943975411 -8))

  (causalt-testbench-calc-fn 'causal-calc--fin-pv-periodic
                             '("--rate=9" "--periods=4" "--amount=2000" "--beginning")
                             '(float 706258933198 -8))

  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--fin-fv-lump ()
  (causalt-calc-setup)

  (causalt-testbench-calc-fn 'causal-calc--fin-fv-lump
                             '("--rate=5.4" "--periods=5" "--amount=5000")
                             '(float 650388807223 -8))

  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--fin-pv-lump ()
  (causalt-calc-setup)

  (causalt-testbench-calc-fn 'causal-calc--fin-pv-lump
                             '("--rate=9" "--periods=4" "--amount=8000")
                             '(float 566740168852 -8))

  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--fin-periodic-payment ()
  (causalt-calc-setup)

  (causalt-testbench-calc-fn 'causal-calc--fin-periodic-payment
                             '("--rate=5.4" "--periods=5" "--amount=1000")
                             '(float 233534637575 -9))

  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--fin-number-of-payments ()
  (causalt-calc-setup)

  (causalt-testbench-calc-fn 'causal-calc--fin-number-of-payments
                             '("--rate=3.5" "--payment=1000" "--amount=10000")
                             '(float 125222398371 -10))

  (causalt-testbench-calc-fn 'causal-calc--fin-number-of-payments
                             '("--rate=3.5" "--payment=1000" "--amount=10000" "--beginning")
                             '(float 119976962243 -10))

  (causalt-calc-breakdown t))


(ert-deftest test-causal-calc--fin-periods-to-reach-target ()
  (causalt-calc-setup)
  (causalt-testbench-calc-fn 'causal-calc--fin-periods-to-reach-target
                             '("--rate=9" "--target=20000" "--deposit=2000")
                             '(float 267190374474 -10))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--fin-rate-of-return ()
  (causalt-calc-setup)
  (causalt-testbench-calc-fn 'causal-calc--fin-rate-of-return
                             '("--periods=4" "--payment=2000" "--pv=6479.44")
                             '(calcFunc-percent
	                       (float 899999827105 -11)))
  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--fin-depreciation-straight-line ()
  (causalt-calc-setup)

  (causalt-testbench-calc-fn 'causal-calc--fin-depreciation-straight-line
                             '("--cost=12000" "--salvage=2000" "--life=5" "--period=0")
                             2000)

  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--fin-depreciation-sum-of-years ()
  (causalt-calc-setup)

  (causalt-testbench-calc-fn 'causal-calc--fin-depreciation-sum-of-years
                             '("--cost=12000" "--salvage=2000" "--life=5" "--period=1")
                             '(float 333333333333 -8))

  (causalt-calc-breakdown t))

(ert-deftest test-causal-calc--fin-depreciation-double-declining-balance ()
  (causalt-calc-setup)
  (causalt-testbench-calc-fn 'causal-calc--fin-depreciation-double-declining-balance
                             '("--cost=12000" "--salvage=2000" "--life=5" "--period=1")
                             4800)
  (causalt-calc-breakdown t))

;; Menus ----

(ert-deftest test-causal-calc-financial-tmenu ()
  (causalt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-percent) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-convert-percent) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-percent-change) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-evaluate) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-refresh) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-last-args) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("f" . causal-calc-fin-pv-fv-tmenu)
                           ("p" . causal-calc-fin-periodic-payments-tmenu)
                           ("n" . causal-calc-fin-number-of-payments-tmenu)
                           ("t" . causal-calc-fin-periods-to-target-tmenu)
                           ("r" . causal-calc-fin-rate-of-return-tmenu)

                           ("%" . causal-calc--percent-of)
                           ("P" . causal-calc--percent)
                           ("c" . calc-convert-percent)
                           ("D" . calc-percent-change)
                           ("=" . calc-evaluate)

                           ("ó" . causal-calc-stack-display-tmenu)
                           ("R" . calc-refresh)
                           ("L" . calc-last-args)
                           ("v" . causal-calc-fin-npv-tmenu)
                           ("i" . causal-calc-fin-irr-tmenu)
                           ("d" . causal-calc-fin-depreciation-tmenu)))
           (test-vectors (append test-vectors causalt-test-basic-operators-group)))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-financial-tmenu
                                       '(lambda () (random 5000)))))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-fin-npv-tmenu ()
  (causalt-calc-setup)
  (let* ((test-vectors '(("n" . causal-calc-fin-npv-tmenu)))
         (test-vectors (append test-vectors causalt-test-basic-operators-group)))
    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-fin-npv-tmenu
                                     '(lambda () (random 5000))))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-fin-pv-fv-tmenu ()
  (causalt-calc-setup)
  (let* ((test-vectors '(("f" . causal-calc--fin-fv-periodic)
                         ("F" . causal-calc--fin-fv-lump)
                         ("p" . causal-calc--fin-pv-periodic)
                         ("P" . causal-calc--fin-pv-lump)))
         (test-vectors (append test-vectors causalt-test-basic-operators-group)))
      (causalt-suffix-testbench-runner test-vectors
                                       #'causal-calc-fin-pv-fv-tmenu
                                       '(lambda () (random 5000))))

  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-fin-periodic-payments-tmenu ()
  (causalt-calc-setup)
  (causalt-testbench-transient-suffix #'causal-calc-fin-periodic-payments-tmenu
                                      "p"
                                      #'causal-calc--fin-periodic-payment
                                      (random 5000))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-fin-number-of-payments-tmenu ()
  (causalt-calc-setup)
  (causalt-testbench-transient-suffix #'causal-calc-fin-number-of-payments-tmenu
                                      "n"
                                      #'causal-calc--fin-number-of-payments
                                      (random 5000))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-fin-rate-of-return-tmenu ()
  (causalt-calc-setup)
  (causalt-testbench-transient-suffix #'causal-calc-fin-rate-of-return-tmenu
                                      "r"
                                      #'causal-calc--fin-rate-of-return
                                      (random 5000))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-fin-periods-to-target-tmenu ()
  (causalt-calc-setup)
  (causalt-testbench-transient-suffix #'causal-calc-fin-periods-to-target-tmenu
                                      "n"
                                      #'causal-calc--fin-periods-to-reach-target
                                      (random 5000))
  (causalt-calc-breakdown t t))

(ert-deftest test-causal-calc-fin-depreciation-tmenu ()
  (causalt-calc-setup)
  (let ((test-vectors '(("1" . causal-calc--fin-depreciation-straight-line)
                        ("2" . causal-calc--fin-depreciation-sum-of-years)
                        ("3" . causal-calc--fin-depreciation-double-declining-balance))))

    (causalt-suffix-testbench-runner test-vectors
                                     #'causal-calc-fin-depreciation-tmenu
                                     '(lambda () (random 5000))))
  (causalt-calc-breakdown t t))

;; (provide 'test-causal-calc-financial)
;;; test-causal-calc-financial.el ends here
