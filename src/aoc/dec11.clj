(ns aoc.dec11
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [aoc.dec02 :as intcode]
            [aoc.dec05]
            [aoc.dec09]))

(defn init-state [mem input]
  {:mem    mem
   :ip     0
   :rb     0
   :input  input
   :output []})

(defn exec-once [init]
  (loop [{:keys [mem ip output input] :as state} init]
    (cond
      (= (get mem ip) 99) [true (assoc state :output [input])]
      (= (count output) 2) [false state]
      :else (recur (intcode/cpu state)))))

(def perform-turn
  {[:up 0]    :left
   [:up 1]    :right
   [:down 0]  :right
   [:down 1]  :left
   [:left 0]  :down
   [:left 1]  :up
   [:right 0] :up
   [:right 1] :down})

(defn move [dir [x y]]
  (case dir
    :up [x (dec y)]
    :down [x (inc y)]
    :left [(dec x) y]
    :right [(inc x) y]))

(defn paint [{:keys [painted pos dir] :as robot} [color turn]]
  (let [next-dir (perform-turn [dir turn])]
    (assoc robot
      :painted (assoc painted pos color)
      :dir next-dir
      :pos (move next-dir pos))))

(defn run-robot [input color]
  (loop [[halt? {:keys [output] :as state}] (exec-once (init-state input color))
         robot {:pos     [0 0]
                :dir     :up
                :painted {[0 0] color}}]
    (cond
      halt? robot
      :else (let [{:keys [painted pos] :as next-robot} (paint robot output)
                  next-input (get painted pos 0)]
              (recur
                (exec-once (assoc state :output [] :input next-input))
                next-robot)))))

(defn print-msg [painted]
  (let [xmin (utils/min-by first (keys painted))
        ymin (utils/min-by second (keys painted))
        xmax (utils/max-by first (keys painted))
        ymax (utils/max-by second (keys painted))]
    (doseq [y (range ymin (inc ymax))]
      (doseq [x (range xmin (inc xmax))]
        (print (if (pos? (get painted [x y] 0)) "X" " ")))
      (println))))

(defn run []
  (let [raw (-> (utils/day-file "11")
                (first)
                (str/split #","))
        input (mapv #(Long/parseLong %) raw)]
    {:part1 (-> (run-robot input 0) :painted keys count)
     :part2 (-> (run-robot input 1) :painted print-msg)}))
