(ns aoc.dec10
  (:require [aoc.utils :as utils]
            [clojure.pprint :refer [pprint]]))

(defn v- [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn vunit [[x y]]
  (Math/atan2 x y))

(defn vlength [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn invert [radians]
  (-> (- radians) (+ Math/PI)))

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
           [p1 (count los)]))
       (sort-by second)
       (last)))

(defn destroy [[result dead] [unit ps]]
  (let [[p & more] ps]
    [(if-not (empty? more)
       (assoc result unit more)
       result)
     (conj dead p)]))

(defn destroy-200 [p1 coords]
  (loop [[to-destroy dead] [(->> coords
                                 (sort-by #(vlength (v- % p1)))
                                 (group-by (comp invert vunit #(v- % p1)))
                                 (into (sorted-map)))
                            []]]
    (if (>= (count dead) 200)
      (dead 199)
      (recur (reduce destroy [(sorted-map) dead] to-destroy)))))


(defn run []
  (let [input (->> (utils/day-file "10")
                   (parse-input))
        [pos result1] (part1 input)]
    {:part1 result1
     :part2 (destroy-200 pos input)}))
