(ns aoc.dec02
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defmulti cpu (fn [[mem ip]]
                (get mem ip)))

(defn arithmetic [mem ip op]
  (let [[_ lx rx ox] (subvec mem ip (+ 4 ip))
        l (get mem lx)
        r (get mem rx)]
    [(assoc mem ox (op l r))
     (+ ip 4)]))

(defmethod cpu 1 [[mem ip]]
  (arithmetic mem ip +))

(defmethod cpu 2 [[mem ip]]
  (arithmetic mem ip *))

(defmethod cpu 99 [x]
  (println "Halting")
  x)

(defn attempt [init noun verb]
  (let [input (-> init
                  (assoc 1 noun)
                  (assoc 2 verb))]
    (loop [[mem ip :as state] [input 0]]
      (if (= (get mem ip) 99)
        (first mem)
        (recur (cpu state))))))

(defn run []
  (let [raw (-> (utils/day-file "02")
                (first)
                (str/split #","))
        input (mapv #(Integer/parseInt %) raw)]
    {:part1 (attempt input 12 2)
     :part2 (for [noun (range 0 100)
                  verb (range 0 100)
                  :when (= (attempt input noun verb) 19690720)]
              [noun verb])}))
