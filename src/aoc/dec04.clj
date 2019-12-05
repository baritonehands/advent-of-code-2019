(ns aoc.dec04
  (:require [aoc.utils :as utils]))

(defn repeats? [digits]
  (->> (partition-by identity digits)
       (map count)
       (some #(> % 1))))

(defn part2-repeats? [digits]
  (->> (partition-by identity digits)
       (map count)
       (some #(= % 2))))

(defn increasing? [digits]
  (apply <= digits))

(defn int->digits [n]
  (mapv #(- (int %) (int \0)) (str n)))

(defn part1-valid? [n]
  (let [digits (int->digits n)]
    (and (repeats? digits)
         (increasing? digits))))

(defn part2-valid? [n]
  (let [digits (int->digits n)]
    (and (part2-repeats? digits)
         (increasing? digits))))

(defn run []
  (let [input (range 367479 893698)]
    {:part1 (->> input
                 (filter part1-valid?)
                 (count))
     :part2 (->> input
                 (filter part2-valid?)
                 (count))}))

