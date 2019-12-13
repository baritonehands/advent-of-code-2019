(ns aoc.dec12
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.string :as s]
            [clojure.math.combinatorics :as combo]))

(def test-input "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>")
(def test-input2 "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>")

(defn extract-var [s]
  (-> s (s/split #"=") (second) (Integer/parseInt)))

(defn parse-input [lines]
  (vec (for [line lines]
         [(-> (str/replace line #"[\<\>\s]" "")
              (str/split #",")
              (->> (mapv extract-var)))
          [0 0 0]])))

(defn v+ [[x1 y1 z1] [x2 y2 z2]]
  [(+ x1 x2) (+ y1 y2) (+ z1 z2)])

(defn energy [[x y z]]
  (+ (Math/abs x) (Math/abs y) (Math/abs z)))

(defn total-energy [[pos vel]]
  (* (energy pos) (energy vel)))

(defn gravity-val [l r]
  (case (compare l r)
    -1 [1 -1]
    1 [-1 1]
    0 [0 0]))

(defn gravity-pair [[lpos lvel] [rpos rvel]]
  (let [[[lx rx] [ly ry] [lz rz]] (map gravity-val lpos rpos)]
    [[lpos (v+ lvel [lx ly lz])]
     [rpos (v+ rvel [rx ry rz])]]))

(defn gravity-all [planets]
  (loop [planets planets
         [[lx rx] & more] (combo/combinations (range 0 4) 2)]
    (if lx
      (let [[l r] (gravity-pair (planets lx) (planets rx))]
        (recur (assoc planets lx l rx r)
               more))
      planets)))

(defn velocity-all [planets]
  (vec (for [[pos vel] planets]
         [(v+ pos vel) vel])))

(defn motion-seq [planets]
  (iterate (comp velocity-all gravity-all) planets))

(defn part1 [planets]
  (->> (motion-seq planets)
       (drop 1000)
       (first)
       (map total-energy)
       (apply +)))

(defn cycle? [planets var-idx]
  (->> (map second planets)
       (every? #(= (get % var-idx) 0))))

(defn var-cycle-count [planets var-idx]
  (->> (map-indexed vector (motion-seq planets))
       (drop 1)
       (drop-while #(not (cycle? (second %) var-idx)))
       (ffirst)))

(defn gcd
  ([a b]
   (if (zero? b) a (recur b (mod a b))))
  ([a b c]
   (gcd a (gcd b c))))

(defn lcm
  ([a b]
   (/ (Math/abs (* a b)) (gcd a b)))
  ([a b c]
   (lcm a (lcm b c))))

(defn part2 [planets]
  (->> (range 0 3)
       (map #(var-cycle-count planets %))
       (apply lcm)
       (* 2)))

(defn run []
  (let [input (->> (utils/day-file "12")
                   ;(str/split test-input #"\n")
                   parse-input)]
    {:part1 (part1 input)
     :part2 (part2 input)}))

