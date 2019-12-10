(ns aoc.dec10
  (:require [aoc.utils :as utils]))

(defn v- [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn vunit [[x y]]
  (Math/atan2 x y))

(defn parse-input [input]
  (set (for [[y row] (map-indexed vector input)
             [x col] (map-indexed vector row)
             :when (= col \#)]
         [x y])))

(defn visible [p1 coords]
  (->> coords
       (map (comp vunit #(v- % p1)))
       (distinct)))

(defn part1 [coords]
  (->> (for [p1 coords]
         (let [los (visible p1 coords)]
           (count los)))
       (sort)
       (last)))

(defn run []
  (let [input (->> (utils/day-file "10")
                   (parse-input))]
    {:part1 (part1 input)}))
