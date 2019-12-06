(ns aoc.dec06
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn parse-orbit [orbit]
  (-> orbit
      (str/split #"\)")
      (reverse)
      (vec)))

(defn sum-orbit [m pair]
  (loop [n 0
         [child parent] pair]
    (if child
      (recur (inc n) (find m parent))
      n)))

(defn part1 [orbits]
  (->> orbits
       (map (partial sum-orbit orbits))
       (reduce +)))

(defn lineage [orbits planet]
  (loop [res []
         parent (get orbits planet)]
    (if parent
      (recur (conj res parent) (get orbits parent))
      res)))

(defn first-common-ancestor [me them]
  (loop [[[idx planet] & more] (map-indexed vector me)]
    (let [them-idx (.indexOf them planet)]
      (if (neg? them-idx)
        (recur more)
        [idx them-idx]))))

(defn part2 [orbits]
  (let [me (lineage orbits "YOU")
        santa (lineage orbits "SAN")
        [me-idx santa-idx] (first-common-ancestor me santa)]
    (+ me-idx santa-idx)))

(defn run []
  (let [orbits (->> (utils/day-file "06")
                    (map parse-orbit)
                    (into {}))]
    {:part1 (part1 orbits)
     :part2 (part2 orbits)}))
