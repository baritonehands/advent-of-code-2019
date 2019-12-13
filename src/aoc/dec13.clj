(ns aoc.dec13
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
      (= (count output) 3) [false state]
      :else (recur (intcode/cpu state)))))

(defn draw [arcade [x y tile-id]]
  (assoc arcade [x y] tile-id))

(defn run-arcade [input]
  (loop [[halt? {:keys [output] :as state}] (exec-once (init-state input 0))
         arcade {}]
    (if halt?
      arcade
      (recur
        (exec-once (assoc state :output []))
        (draw arcade output)))))

(defn move-joystick [{:keys [ball paddle] :as game}]
  (assoc
    game
    :joystick
    (cond
      (or (nil? ball) (nil? paddle)) 0
      (> (first ball) (first paddle)) 1
      (< (first ball) (first paddle)) -1
      :else 0)))

(defn print-screen [screen]
  (let [xmin (utils/min-by first (keys screen))
        ymin (utils/min-by second (keys screen))
        xmax (utils/max-by first (keys screen))
        ymax (utils/max-by second (keys screen))]
    (doseq [y (range ymin (inc ymax))]
      (doseq [x (range xmin (inc xmax))]
        (print (case (get screen [x y] 0)
                 1 "#"
                 2 "="
                 3 "_"
                 4 "o"
                 " ")))
      (println))))

(defn play [game [x y tile-id]]
  (let [next-game (cond
                    (= [x y] [-1 0]) (assoc game :score tile-id)
                    (= tile-id 4) (assoc game :ball [x y])
                    (= tile-id 3) (assoc game :paddle [x y])
                    :else game)]
    (-> next-game
        (update :screen assoc [x y] tile-id)
        (move-joystick))))

(defn run-game [input]
  (loop [[halt? {:keys [output] :as state}] (exec-once (init-state (assoc input 0 2) 0))
         game {:paddle   nil
               :ball     nil
               :score    0
               :screen   {}
               :joystick 0}]
    (if halt?
      game
      (let [next-game (play game output)]
        (recur
          (exec-once (assoc state :output [] :input (:joystick next-game)))
          next-game)))))


(defn run []
  (let [raw (-> (utils/day-file "13")
                (first)
                (str/split #","))
        input (mapv #(Long/parseLong %) raw)]
    {:part1 (->> (run-arcade input)
                 (vals)
                 (filter #{2})
                 (count))
     :part2 (-> (run-game input)
                (doto (-> :screen print-screen))
                :score)}))
