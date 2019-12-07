(ns aoc.dec07
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [aoc.dec02 :as intcode]
            [aoc.dec05]))

(def test-input1 "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")

(defn run-phase [mem phase input]
  (-> (intcode/exec mem [phase input])
      :output
      first))

(defn part1 [mem phases]
  (reduce
    (fn [input phase]
      (run-phase mem phase input))
    0
    phases))

(defn run []
  (let [raw (-> (utils/day-file "07")
                (first)
                ;test-input1
                (str/split #","))
        input (mapv #(Integer/parseInt %) raw)]
    {:part1 (->> (combo/permutations (range 0 5))
                 (map #(part1 input %))
                 (apply max))}))
