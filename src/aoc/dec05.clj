(ns aoc.dec05
  (:require [aoc.dec02 :as intcode]
            [aoc.utils :as utils]
            [clojure.string :as str]))

(defmethod intcode/cpu 3 [{:keys [mem ip input] :as state}]
  (let [[opcode param] (subvec mem ip)
        [mode] (intcode/opcode->modes opcode)]
    (assert (zero? mode))
    (assert (some? input))
    (assoc state
      :mem (assoc mem param (if (sequential? input)
                              (first input)
                              input))
      :ip (+ ip 2)
      :input (if (sequential? input)
               (next input)
               input))))

(defmethod intcode/cpu 4 [{:keys [mem ip output] :as state}]
  (let [[opcode param] (subvec mem ip)
        [mode] (intcode/opcode->modes opcode)]
    (assoc state
      :output (conj output (intcode/get-param mode mem param))
      :ip (+ ip 2))))

(defn jump [pred {:keys [mem ip] :as state}]
  (let [[opcode test next-ip] (subvec mem ip)
        [tm im] (intcode/opcode->modes opcode)]
    (if (pred (intcode/get-param tm mem test))
      (assoc state :ip (intcode/get-param im mem next-ip))
      (update state :ip #(+ % 3)))))

(defmethod intcode/cpu 5 [state]
  (jump (complement zero?) state))

(defmethod intcode/cpu 6 [state]
  (jump zero? state))

(defn comparison [f {:keys [mem ip] :as state}]
  (let [[opcode lx rx ox] (subvec mem ip)
        [lm rm om] (intcode/opcode->modes opcode)
        l (intcode/get-param lm mem lx)
        r (intcode/get-param rm mem rx)]
    (assert (zero? om))
    (assoc state
      :mem (assoc mem ox (if (f l r) 1 0))
      :ip (+ ip 4))))

(defmethod intcode/cpu 7 [state]
  (comparison < state))

(defmethod intcode/cpu 8 [state]
  (comparison = state))

(defn run []
  (let [raw (-> (utils/day-file "05")
                (first)
                (str/split #","))
        input (mapv #(Integer/parseInt %) raw)]
    {:part1 (:output (intcode/exec input 1))
     :part2 (:output (intcode/exec input 5))}))
