(ns aoc.dec03
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn dir [xs ys]
  (for [x xs
        y ys]
    [x y]))

(defn vrange [[x y] [ch n]]
  (case ch
    \U (dir [x] (range (dec y) (- y n 1) -1))
    \D (dir [x] (range (inc y) (+ y n 1)))
    \L (dir (range (dec x) (- x n 1) -1) [y])
    \R (dir (range (inc x) (+ x n 1)) [y])))

(defn path [ps]
  (loop [[p & more] ps
         pos [0 0]
         res []]
    (if p
      (let [r (vrange pos p)]
        (recur more (last r) (into res r)))
      res)))

(defn move [s]
  [(.charAt s 0) (Integer/parseInt (.substring s 1))])

(defn line->path [line]
  (let [moves (->> (str/split line #",")
                   (mapv move))]
    (path moves)))

(defn part1 [[p1 p2]]
  (->> (set/intersection
         (-> p1 line->path set)
         (-> p2 line->path set))
       (map #(utils/ny-distance [0 0] %))
       (sort)))

(defn intersection-idx [m [p n]]
  (update m p (fnil conj []) n))

(defn line->path+idx [line]
  (->> (map vector (line->path line) (map inc (range)))
       (reduce intersection-idx {})))

(defn part2 [[p1 p2]]
  (let [l (line->path+idx p1)
        r (line->path+idx p2)
        ks (set/intersection (-> l keys set) (-> r keys set))]
    (->> (for [k ks]
           [k (+ (first (l k)) (first (r k)))])
         (sort-by second))))

(defn run []
  (let [input (utils/day-file "03")]
    {:part1 (part1 input)
     :part2 (part2 input)}))
