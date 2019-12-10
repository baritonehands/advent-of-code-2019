(ns aoc.dec02
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn opcode->instruction [opcode]
  (mod opcode 100))

(defmulti cpu (fn [{:keys [mem ip] :as state}]
                (opcode->instruction (get mem ip))))

(defn lpad [s]
  (let [pad (- 3 (count s))]
    (->> (concat (repeat pad \0) s)
         (map #(- (int %) (int \0)))
         (reverse)
         (vec))))

(defn opcode->modes [opcode]
  (-> opcode (/ 100) int str lpad))

(defn get-param [mode {:keys [mem rb]} idx]
  (case mode
    0 (get mem idx 0)
    1 idx
    2 (get mem (+ rb idx) 0)))

(defn set-param [mode {:keys [mem rb]} idx v]
  (let [addr (if (zero? mode) idx (+ rb idx))
        mem (if (> (count mem) addr)
              mem
              (->> (concat mem (repeat 0))
                   (take (inc addr))
                   (vec)))]
    (assoc mem addr v)))

(defn arithmetic [{:keys [mem ip] :as state} op]
  (let [[opcode lx rx ox] (subvec mem ip (+ 4 ip))
        [lm rm om] (opcode->modes opcode)
        l (get-param lm state lx)
        r (get-param rm state rx)]
    (assert (#{0 2} om))
    (assoc state
      :mem (set-param om state ox (op l r))
      :ip (+ ip 4))))

(defmethod cpu 1 [state]
  (arithmetic state +))

(defmethod cpu 2 [state]
  (arithmetic state *))

(defn exec [mem input]
  (loop [{:keys [mem ip] :as state} {:mem    mem
                                     :ip     0
                                     :rb     0
                                     :input  input
                                     :output []}]
    (if (= (get mem ip) 99)
      state
      (recur (cpu state)))))

(defn attempt [init noun verb]
  (let [input (-> init
                  (assoc 1 noun)
                  (assoc 2 verb))]
    (-> (exec input nil) :mem first)))

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
