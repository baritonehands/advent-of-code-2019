(ns aoc.dec09
  (:require [aoc.utils :as utils]
            [aoc.dec02 :as intcode]
            [aoc.dec05]
            [clojure.string :as str]))

(defmethod intcode/cpu 9 [{:keys [mem ip rb] :as state}]
  (let [[opcode param] (subvec mem ip)
        [mode] (intcode/opcode->modes opcode)
        v (intcode/get-param mode state param)]
    (assoc state :rb (+ rb v) :ip (+ ip 2))))

(defn run []
  (let [raw (-> (utils/day-file "09")
                (first)
                (str/split #","))
        input (mapv #(Long/parseLong %) raw)]
    {:part1 (->> (intcode/exec input 1)
                 (time)
                 :output)
     :part2 (->> (intcode/exec input 2)
                 (time)
                 :output)}))
