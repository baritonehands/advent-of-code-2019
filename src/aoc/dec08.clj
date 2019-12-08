(ns aoc.dec08
  (:require [aoc.utils :as utils]))

(def width 25)
(def height 6)

(defn parse-layers [input]
  (->> (partition (* width height) input)
       (map vec)))

(defn part1 [layers]
  (let [least-0 (->> layers
                     (map frequencies)
                     (sort-by #(% \0))
                     (first))]
    (* (least-0 \1) (least-0 \2))))

(defn get-pixel [layers idx]
  (loop [[layer & more] layers]
    (cond
      (nil? layer) \2
      (= (layer idx) \2) (recur more)
      :else (layer idx))))

(defn point->idx [x y]
  (+ (* y width) x))

(defn draw-img [img]
  (doseq [y (range 0 height)]
    (doseq [x (range 0 width)]
      (let [pixel (img (point->idx x y))]
        (print (if (= pixel \0) " " "X"))))
    (println)))

(defn part2 [layers]
  (->> (for [y (range 0 height)
             x (range 0 width)]
         (let [idx (point->idx x y)]
           (get-pixel layers idx)))
       vec))

(defn run []
  (let [input (->> (utils/day-file "08")
                   (first)
                   (parse-layers))]
    {:part1 (part1 input)
     :part2 (->> (part2 input)
                 (draw-img))}))
