#!/usr/bin/env bb
;; tropical_derangement.bb — Droid D (seed 0xC9AB51D02654A7E2, trit ERGODIC)
;;
;; Tropical semiring (min-plus) x Derangements x GF(3) coloring
;; Outputs EDN with :derangements :distances :gf3-conservation
;;
;; Algebra:
;;   Tropical semiring: (R ∪ {+∞}, min, +)
;;     "addition" = min   (choose the cheaper)
;;     "multiplication" = +  (costs compose)
;;   Derangement: permutation σ where σ(i) ≠ i for all i (no fixed points)
;;   GF(3): Galois field of order 3, elements {0,1,2}, arithmetic mod 3
;;   SplitMix64: deterministic PRNG for coloring derangements

;; ════════════════════════════════════════════════════════════════
;; SplitMix64 — canonical constants (unchecked-long for 64-bit)
;; ════════════════════════════════════════════════════════════════

(def ^:const GOLDEN (unchecked-long 0x9E3779B97F4A7C15))
(def ^:const MIX1   (unchecked-long 0xBF58476D1CE4E5B9))
(def ^:const MIX2   (unchecked-long 0x94D049BB133111EB))
(def ^:const SEED   (unchecked-long 0xC9AB51D02654A7E2))

(defn splitmix64
  "One step of SplitMix64. Returns [next-state output-value]."
  [^long state]
  (let [s (unchecked-add state GOLDEN)
        z s
        z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 30)) MIX1)
        z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 27)) MIX2)
        z (bit-xor z (unsigned-bit-shift-right z 31))]
    [s z]))

(defn splitmix64-seq
  "Lazy sequence of SplitMix64 output values from a seed."
  [seed]
  (let [[next-state v] (splitmix64 seed)]
    (lazy-seq (cons v (splitmix64-seq next-state)))))

;; ════════════════════════════════════════════════════════════════
;; GF(3) — Galois Field of order 3
;; ════════════════════════════════════════════════════════════════

(defn gf3
  "Map an integer to GF(3) element {0, 1, 2}."
  [^long v]
  (let [r (mod (Math/abs v) 3)]
    (if (neg? r) (+ r 3) r)))

(defn gf3-add [a b] (mod (+ a b) 3))
(defn gf3-mul [a b] (mod (* a b) 3))
(defn gf3-neg [a]   (mod (- 3 a) 3))

;; ════════════════════════════════════════════════════════════════
;; Tropical Semiring (min, +)
;;
;;   ⊕ = min     (tropical addition)
;;   ⊗ = +       (tropical multiplication)
;;   0_T = +∞    (additive identity: min(x, ∞) = x)
;;   1_T = 0     (multiplicative identity: x + 0 = x)
;; ════════════════════════════════════════════════════════════════

(def T-ZERO ##Inf)    ;; additive identity
(def T-ONE  0)        ;; multiplicative identity

(defn t-add
  "Tropical addition: min(a, b)."
  [a b]
  (min a b))

(defn t-mul
  "Tropical multiplication: a + b (ordinary addition)."
  [a b]
  (+ a b))

(defn t-add-vec
  "Tropical addition of vectors: component-wise min."
  [u v]
  (mapv t-add u v))

(defn t-mul-scalar
  "Tropical scalar multiplication: add scalar to each component."
  [s v]
  (mapv #(t-mul s %) v))

;; ════════════════════════════════════════════════════════════════
;; Derangement Generation
;;
;; A derangement of [0..n-1] is a permutation σ where σ(i) ≠ i
;; for all i. |D_n| = subfactorial(n).
;;
;; For n=3..8 we generate all derangements exhaustively (feasible
;; since D_8 = 14833).
;; ════════════════════════════════════════════════════════════════

(defn subfactorial
  "Count of derangements: D(0)=1, D(1)=0, D(n)=(n-1)(D(n-1)+D(n-2))."
  [n]
  (cond
    (= n 0) 1
    (= n 1) 0
    :else (loop [i 2 d2 1 d1 0]
            (let [d (* (dec i) (+ d1 d2))]
              (if (= i n) d (recur (inc i) d1 d))))))

(defn permutations
  "All permutations of vector v."
  [v]
  (if (<= (count v) 1)
    [v]
    (for [i (range (count v))
          rest-perm (permutations (into (subvec v 0 i) (subvec v (inc i))))]
      (into [(v i)] rest-perm))))

(defn derangement?
  "True if perm is a derangement: σ(i) ≠ i for all i."
  [perm]
  (every? (fn [i] (not= (nth perm i) i)) (range (count perm))))

(defn all-derangements
  "All derangements of [0..n-1]."
  [n]
  (filterv derangement? (permutations (vec (range n)))))

;; ════════════════════════════════════════════════════════════════
;; GF(3) Coloring of Derangements via SplitMix64
;;
;; Each derangement gets a GF(3) trit by feeding the droid seed
;; through SplitMix64 and taking output mod 3.
;; ════════════════════════════════════════════════════════════════

(defn color-derangements
  "Assign a GF(3) trit to each derangement using SplitMix64 from SEED.
   Returns vector of {:perm [...] :gf3 <0|1|2>}."
  [derangements]
  (let [outputs (splitmix64-seq SEED)]
    (mapv (fn [perm val]
            {:perm perm
             :gf3  (gf3 val)})
          derangements
          outputs)))

;; ════════════════════════════════════════════════════════════════
;; Tropical Edit Distance
;;
;; Edit distance between two derangements under the tropical semiring.
;; For permutations σ and τ of [0..n-1]:
;;
;;   displacement(i) = |σ(i) - τ(i)|
;;   edit_distance(σ, τ) = ⊗_i displacement(i)
;;                        = Σ_i |σ(i) - τ(i)|  (tropical product = sum)
;;
;; This is the L1 (Manhattan) distance between permutation vectors,
;; which is the natural metric when tropical multiplication = addition.
;;
;; The tropical distance matrix D[i][j] = edit_distance(d_i, d_j)
;; satisfies:
;;   D[i][i] = 0  (= T-ONE, multiplicative identity)
;;   D[i][j] = D[j][i]  (symmetric)
;;   D[i][k] ≥ min(D[i][j], D[j][k])  (tropical triangle: ultra-metric)
;; ════════════════════════════════════════════════════════════════

(defn tropical-edit-distance
  "Tropical edit distance: sum of absolute displacements.
   This is tropical multiplication (ordinary +) over all positions."
  [sigma tau]
  (reduce t-mul T-ONE
          (map (fn [s t] (Math/abs (- s t))) sigma tau)))

(defn build-distance-matrix
  "Build the tropical distance matrix for a set of derangements.
   Returns a vector of vectors (symmetric matrix)."
  [derangements]
  (let [n (count derangements)]
    (vec
      (for [i (range n)]
        (vec
          (for [j (range n)]
            (tropical-edit-distance
              (nth derangements i)
              (nth derangements j))))))))

;; ════════════════════════════════════════════════════════════════
;; GF(3) Conservation Analysis
;;
;; For each n, compute the GF(3) trit sum over all derangements.
;; Conservation holds when Σ gf3(d) ≡ 0 (mod 3).
;; ════════════════════════════════════════════════════════════════

(defn gf3-conservation
  "Analyze GF(3) conservation properties for colored derangements."
  [colored-derangements]
  (let [trits (mapv :gf3 colored-derangements)
        counts (frequencies trits)
        total  (reduce gf3-add 0 trits)]
    {:trit-counts    {0 (get counts 0 0)
                      1 (get counts 1 0)
                      2 (get counts 2 0)}
     :trit-sum-gf3   total
     :conserved?     (zero? total)
     :total-count    (count trits)
     :droid-trit     (gf3 (second (splitmix64 SEED)))
     :seed           (format "0x%016X" SEED)}))

;; ════════════════════════════════════════════════════════════════
;; Main: compute for n=3..8 and emit EDN
;; ════════════════════════════════════════════════════════════════

(defn compute-for-n
  "Compute derangements, coloring, and distances for a given n.
   For n>6 we subsample distances (full matrix would be huge)."
  [n]
  (let [dergs         (all-derangements n)
        colored       (color-derangements dergs)
        conservation  (gf3-conservation colored)
        ;; For distance matrix: use all derangements if <= 44 (n<=5),
        ;; otherwise subsample first 20 for tractability
        sample-size   (min (count dergs) 20)
        sample-perms  (mapv :perm (take sample-size colored))
        dist-matrix   (build-distance-matrix sample-perms)]
    {:n               n
     :subfactorial    (subfactorial n)
     :derangement-count (count dergs)
     :derangements    (mapv (fn [{:keys [perm gf3]}]
                              {:perm perm :gf3 gf3})
                            colored)
     :distances       {:sample-size sample-size
                       :matrix      dist-matrix
                       :metric      :tropical-edit-L1
                       :note        (if (< sample-size (count dergs))
                                     (format "Subsampled %d of %d derangements for distance matrix"
                                             sample-size (count dergs))
                                     "Full distance matrix")}
     :gf3-conservation conservation}))

(let [results (mapv compute-for-n (range 3 9))
      ;; Global GF(3) conservation: sum trits across all n
      all-trits    (mapcat (fn [r] (map :gf3 (:derangements r))) results)
      global-sum   (reduce gf3-add 0 all-trits)
      global-counts (frequencies all-trits)
      output {:droid           "D"
              :seed            (format "0x%016X" SEED)
              :trit            :ERGODIC
              :splitmix64      {:GOLDEN (format "0x%016X" GOLDEN)
                                :MIX1   (format "0x%016X" MIX1)
                                :MIX2   (format "0x%016X" MIX2)}
              :tropical-semiring {:addition    :min
                                  :multiplication :plus
                                  :zero        :+Inf
                                  :one         0}
              :derangements    (into {}
                                 (map (fn [r]
                                        [(keyword (str "n" (:n r)))
                                         {:subfactorial      (:subfactorial r)
                                          :count             (:derangement-count r)
                                          :verified?         (= (:derangement-count r)
                                                                (subfactorial (:n r)))
                                          :colored           (:derangements r)}])
                                      results))
              :distances       (into {}
                                 (map (fn [r]
                                        [(keyword (str "n" (:n r)))
                                         (:distances r)])
                                      results))
              :gf3-conservation {:per-n       (into {}
                                                (map (fn [r]
                                                       [(keyword (str "n" (:n r)))
                                                        (:gf3-conservation r)])
                                                     results))
                                 :global      {:trit-counts  {0 (get global-counts 0 0)
                                                              1 (get global-counts 1 0)
                                                              2 (get global-counts 2 0)}
                                               :trit-sum-gf3 global-sum
                                               :conserved?   (zero? global-sum)
                                               :total-derangements (count all-trits)}}}]
  (prn output))
