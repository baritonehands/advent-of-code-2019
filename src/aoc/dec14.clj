(ns aoc.dec14
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def test-input "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL")
(def test-input2 "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")
(def test-input3 "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n17 NVRVD, 3 JNWZP => 8 VPVL\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n22 VJHF, 37 MNCFX => 5 FWMGM\n139 ORE => 4 NVRVD\n144 ORE => 7 JNWZP\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n145 ORE => 6 MNCFX\n1 NVRVD => 8 CXFTF\n1 VJHF, 6 MNCFX => 4 RFSQX\n176 ORE => 6 VJHF")
(def test-input4 "171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX")

(defn parse-pair [s]
  (let [[amt name] (str/split s #"\s+")]
    [(keyword name) (Integer/parseInt amt)]))

(defn parse-input [lines]
  (->> (for [line lines]
         (let [[formula result] (str/split line #"\s+=>\s+")
               [material amt] (parse-pair result)]
           [material
            [amt (->> (str/split formula #",\s+") (mapv parse-pair))]]))
       (into {})))

(defn make [[formulas leftover] [material cnt]]
  (if (= material :ORE)
    cnt
    (let [[amt ingredients] (formulas material)
          reactions (-> (/ cnt amt) (Math/ceil) int)]
      (->> ingredients
           (map #(make [formulas leftover] %))
           (reduce +)
           (* reactions)))))

(defn amount [n [material cnt]]
  [material [(* cnt n) 0]])

(defn add-req [[lcnt lleft] [rcnt rleft]]
  (println lcnt lleft rcnt rleft)
  [(+ lcnt rcnt) (+ lleft rleft)])

(defn make2 [formulas requirements]
  (->> (for [[material [cnt left]] requirements]
         (if (= material :ORE)
           {:ORE [cnt left]}
           (let [[amt ingredients] (formulas material)
                 reactions (-> (/ (- cnt left) amt) (Math/ceil) int) ;(max 0))
                 leftovers {material [0 (max (- (* amt reactions) cnt) 0)]}]
             (if (zero? reactions)
               {material [0 (- left cnt)]}
               (->> (map #(amount reactions %) ingredients)
                    (into leftovers))))))
       (apply merge-with add-req)))

(defn reacting? [requirements]
  (some (fn [[material [req]]]
          (and (not= material :ORE)
               (pos? req)))
        requirements))

(defn part1 [formulas]
  (println formulas)
  (->> {:FUEL [1 0]}
       (iterate #(make2 formulas %))
       (drop-while reacting?)
       (first)))

(defn run []
  (let [input (->> (utils/day-file "14")
                   ;(str/split test-input4 #"\n")
                   (parse-input))]
    {:part1 (part1 input)}))
