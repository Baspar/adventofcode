(require '[clojure.string :refer [split join]])
(require '[clojure.pprint :refer [pprint]])

;; Absolute Value Function
(defn abs [n] (max n (- n)))
(defn dist [x y] (+ (abs x) (abs y)))

;; Inputs
(def inputs "R4, R4, L1, R3, L5, R2, R5, R1, L4, R3, L5, R2, L3, L4, L3, R1, R5, R1, L3, L1, R3, L1, R2, R2, L2, R5, L3, L4, R4, R4, R2, L4, L1, R5, L1, L4, R4, L1, R1, L2, R5, L2, L3, R2, R1, L194, R2, L4, R49, R1, R3, L5, L4, L1, R4, R2, R1, L5, R3, L5, L4, R4, R4, L2, L3, R78, L5, R4, R191, R4, R3, R1, L2, R1, R3, L1, R3, R4, R2, L2, R1, R4, L5, R2, L2, L4, L2, R1, R2, L3, R5, R2, L3, L3, R3, L1, L1, R5, L4, L4, L2, R5, R1, R4, L3, L5, L4, R5, L4, R5, R4, L3, L2, L5, R4, R3, L3, R1, L5, R5, R1, L3, R2, L5, R5, L3, R1, R4, L5, R4, R2, R3, L4, L5, R3, R4, L5, L5, R4, L4, L4, R1, R5, R3, L1, L4, L3, L4, R1, L5, L1, R2, R2, R4, R4, L5, R4, R1, L1, L1, L3, L5, L2, R4, L3, L5, L4, L1, R3")

;; Splitting the input in an arrray
(def instructions (split inputs ", "))

;; Transforming each instructions
;; from:
;;    R4
;; to:
;;   [R F F F]
;; (Standing for Right Front Front Front)
(defn split-instruction [instruction]
  (let [turn (first instruction)
        dist (js/parseInt (join "" (rest instruction)))

        turn-path turn
        walk-path (repeat (dec dist) "F")

        full-path (conj walk-path turn-path)]
    full-path))
(def instructions (map split-instruction instructions))

;; Concat all splitted instructions
(def instructions (apply concat instructions))


;; Function to simulate one step
;; Also print if a location has already been visited
(defn apply-move [{x :x y :y [xd yd] :dir seen :seen} turn]
  (let [new-dir (case turn
                  "F" [xd yd]
                  "R" [(- yd) xd]
                  "L" [yd (- xd)])
        new-xd (first new-dir)
        new-yd (second new-dir)
        new-x (+ x new-xd)
        new-y (+ y new-yd)

        new-seen (conj seen [new-x new-y])

        out { :x new-x :y new-y :dir new-dir :seen new-seen}]

    (when (contains? seen [new-x new-y])
      (println "Passing twice or more on:" [new-x new-y]))
    out))

;; Simulating
(def res (reduce apply-move
                 {:x 0
                  :y 0
                  :dir [0 1]
                  :seen #{[0 0]}}
                 instructions))

(print "Final distance:" (dist (:x res) (:y res)))

