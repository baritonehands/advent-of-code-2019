(ns aoc.dec07
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [aoc.dec02 :as intcode]
            [aoc.dec05]))

(def test-input1 "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
(def test-input2 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
(def test-input3 "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")

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

(defn init-state [mem input]
  {:mem    mem
   :ip     0
   :input  input
   :output []})

(defn exec-once [init]
  (println init)
  (loop [{:keys [mem ip output input] :as state} init]
    (cond
      (= (get mem ip) 99) [true (assoc state :output [input])]
      (seq output) [false state]
      :else (recur (intcode/cpu state)))))

(defn exec-first [mem phase input]
  (exec-once (init-state mem [phase input])))

(defn part2 [mem phases]
  (loop [amps [(exec-first mem (phases 0) 0)]
         cur 0]
    (let [[halt? {:keys [output] :as state}] (amps cur)
          next-idx (mod (inc cur) 5)
          [_ next-amp] (get amps next-idx)]
      (run! println amps)
      (println cur "\n")
      (cond
        (and halt? (zero? next-idx))
        (first output)

        (nil? next-amp)
        (recur
          (conj amps (exec-first mem (phases next-idx) (first output)))
          next-idx)

        :else
        (recur
          (assoc amps next-idx (exec-once
                                 (assoc next-amp :output [] :input (first output))))
          next-idx)))))

(defn run []
  (let [raw (-> (utils/day-file "07")
                (first)
                ;test-input3
                (str/split #","))
        input (mapv #(Integer/parseInt %) raw)]
    {
     ;:part1 (->> (combo/permutations (range 0 5))
     ;            (map #(part1 input %))
     ;            (apply max))
     :part2 (part2 input [5 9 6 8 7])}))
            ;(->> (combo/permutations (range 5 10))
            ;     (map (juxt identity #(part2 input %)))
            ;     (sort-by second))}))
