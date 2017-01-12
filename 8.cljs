(require '[clojure.string :refer [split join]])
(require '[clojure.pprint :refer [pprint]])

(def inputs "rect 1x1
rotate row y=0 by 5
rect 1x1
rotate row y=0 by 6
rect 1x1
rotate row y=0 by 5
rect 1x1
rotate row y=0 by 2
rect 1x1
rotate row y=0 by 5
rect 2x1
rotate row y=0 by 2
rect 1x1
rotate row y=0 by 4
rect 1x1
rotate row y=0 by 3
rect 2x1
rotate row y=0 by 7
rect 3x1
rotate row y=0 by 3
rect 1x1
rotate row y=0 by 3
rect 1x2
rotate row y=1 by 13
rotate column x=0 by 1
rect 2x1
rotate row y=0 by 5
rotate column x=0 by 1
rect 3x1
rotate row y=0 by 18
rotate column x=13 by 1
rotate column x=7 by 2
rotate column x=2 by 3
rotate column x=0 by 1
rect 17x1
rotate row y=3 by 13
rotate row y=1 by 37
rotate row y=0 by 11
rotate column x=7 by 1
rotate column x=6 by 1
rotate column x=4 by 1
rotate column x=0 by 1
rect 10x1
rotate row y=2 by 37
rotate column x=19 by 2
rotate column x=9 by 2
rotate row y=3 by 5
rotate row y=2 by 1
rotate row y=1 by 4
rotate row y=0 by 4
rect 1x4
rotate column x=25 by 3
rotate row y=3 by 5
rotate row y=2 by 2
rotate row y=1 by 1
rotate row y=0 by 1
rect 1x5
rotate row y=2 by 10
rotate column x=39 by 1
rotate column x=35 by 1
rotate column x=29 by 1
rotate column x=19 by 1
rotate column x=7 by 2
rotate row y=4 by 22
rotate row y=3 by 5
rotate row y=1 by 21
rotate row y=0 by 10
rotate column x=2 by 2
rotate column x=0 by 2
rect 4x2
rotate column x=46 by 2
rotate column x=44 by 2
rotate column x=42 by 1
rotate column x=41 by 1
rotate column x=40 by 2
rotate column x=38 by 2
rotate column x=37 by 3
rotate column x=35 by 1
rotate column x=33 by 2
rotate column x=32 by 1
rotate column x=31 by 2
rotate column x=30 by 1
rotate column x=28 by 1
rotate column x=27 by 3
rotate column x=26 by 1
rotate column x=23 by 2
rotate column x=22 by 1
rotate column x=21 by 1
rotate column x=20 by 1
rotate column x=19 by 1
rotate column x=18 by 2
rotate column x=16 by 2
rotate column x=15 by 1
rotate column x=13 by 1
rotate column x=12 by 1
rotate column x=11 by 1
rotate column x=10 by 1
rotate column x=7 by 1
rotate column x=6 by 1
rotate column x=5 by 1
rotate column x=3 by 2
rotate column x=2 by 1
rotate column x=1 by 1
rotate column x=0 by 1
rect 49x1
rotate row y=2 by 34
rotate column x=44 by 1
rotate column x=40 by 2
rotate column x=39 by 1
rotate column x=35 by 4
rotate column x=34 by 1
rotate column x=30 by 4
rotate column x=29 by 1
rotate column x=24 by 1
rotate column x=15 by 4
rotate column x=14 by 1
rotate column x=13 by 3
rotate column x=10 by 4
rotate column x=9 by 1
rotate column x=5 by 4
rotate column x=4 by 3
rotate row y=5 by 20
rotate row y=4 by 20
rotate row y=3 by 48
rotate row y=2 by 20
rotate row y=1 by 41
rotate column x=47 by 5
rotate column x=46 by 5
rotate column x=45 by 4
rotate column x=43 by 5
rotate column x=41 by 5
rotate column x=33 by 1
rotate column x=32 by 3
rotate column x=23 by 5
rotate column x=22 by 1
rotate column x=21 by 2
rotate column x=18 by 2
rotate column x=17 by 3
rotate column x=16 by 2
rotate column x=13 by 5
rotate column x=12 by 5
rotate column x=11 by 5
rotate column x=3 by 5
rotate column x=2 by 5
rotate column x=1 by 5")

(defn rect [w h grid]
  (map-indexed (fn [y row]
                 (map-indexed (fn [x cell]
                                (if (and (< x w) (< y h))
                                  "x"
                                  cell))
                              row))
               grid))
(defn rotate-row [row-index by grid]
  (map-indexed (fn [index row]
                 (if (= index row-index)
                   (let [nb (count row)
                         split (split-at (mod (- nb by) nb) row)
                         return (apply concat (reverse split))]
                     return)
                   row))
               grid))
(defn rotate-col [col by grid]
  (->> grid
       (apply mapv vector)
       (rotate-row col by)
       (apply mapv vector)))
(defn new-grid [w h]
  (repeat h
          (repeat w 0)))

(def instructions (map #(split % #" ") (split inputs #"\n")))
(def raw-grid ())

(defn display! [grid]
  (print
    (join
      "\n"
      (map (fn [row]
           (apply str
                  (map (fn [character] (if (zero? character)
                                         "."
                                         "x"))
                       row)))
         grid))))

(defn apply-instruction [grid instruction]
  (display! grid)
  (let [type (first instruction)]
    (if (= "rect" type)
      (let [[w h] (split (second instruction) #"x")]
        (rect w h grid))
      (let [dir (second instruction)
            pos (js/parseInt (last (split (nth instruction 2) #"=")))
            nb (js/parseInt (last instruction))]
        (if (= "column" dir)
          (rotate-col pos nb grid)
          (rotate-row pos nb grid))))))

(defn apply-instructions [grid instructions]
  (reduce apply-instruction grid instructions))

(defn nb-cell-on [grid]
  (reduce +
          (map #(reduce (fn [nb cell] (if (= "x" cell)
                                        (inc nb)
                                        nb))
                        0 %)
               grid)))

(def grid-after-instructions (apply-instructions (new-grid 50 6) instructions))

(pprint (nb-cell-on grid-after-instructions))

(display! grid-after-instructions)
