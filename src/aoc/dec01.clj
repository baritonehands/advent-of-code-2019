(ns aoc.dec01
  (:require [aoc.utils :as utils]))

(defn fuel [mass]
  (let [ret (-> mass
                (/ 3)
                (double)
                (Math/floor)
                (- 2))]
    (if (pos? ret) ret 0)))

(defn mass+fuel [mass]
  (let [mfuel (fuel mass)]
    (loop [ffuel (fuel mfuel)
           total (+ mfuel ffuel)]
      (if (<= ffuel 0)
        total
        (recur (fuel ffuel)
               (+ total (fuel ffuel)))))))

(defn run []
  (let [input (->> (utils/day-file "01")
                   (mapv #(Integer/parseInt %)))]
    {:part1 (->> input
                 (mapv fuel)
                 (reduce +))
     :part2 (->> input
                 (mapv mass+fuel)
                 (reduce +))}))
