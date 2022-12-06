(ns aoc-2022.day6 
  (:require [aoc-2022.utils :as utils]))

(defn processchar [datastream i foundIndex countback]
  (let [
        prevchars (subs datastream (- i countback) i)
        maxcommoncharcount (reduce max (vals (frequencies prevchars)))
        ]
    (when (= maxcommoncharcount 1) (swap! foundIndex (constantly i)))))

(defn findpacketstartindex [datastream countback] 
  (let [
        indexedchars (vec (map-indexed vector datastream))
        foundIndex   (atom 0)
        ]
    (doseq [[i] indexedchars]
      (when (and 
             (= 0 @foundIndex) 
             (>= i countback)) 
        (processchar datastream i foundIndex countback)
        )
      )
    @foundIndex))

(defn part1 []
  (let
   [datastream (first (utils/readlines "resources/day6/input.txt"))]
    (findpacketstartindex datastream 4)))

(defn part2 []
  (let
   [datastream (first (utils/readlines "resources/day6/input.txt"))]
    (findpacketstartindex datastream 14)))