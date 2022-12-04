(ns aoc-2022.day4
  (:require [aoc-2022.utils :as utils]
            [clojure.string :refer [split]]))

(defn parseranges 
  [line]
  (let 
   [linesplit (split line #",")
    firstrange (first linesplit) 
    secondrange (second linesplit)
    firstrangesplit (split firstrange #"-")
    secondrangesplit (split secondrange #"-")
    lb1    (Integer/parseInt (first firstrangesplit))
    ub1    (Integer/parseInt (second firstrangesplit))
    lb2    (Integer/parseInt (first secondrangesplit))
    ub2    (Integer/parseInt (second secondrangesplit))
    ]
    [lb1 ub1 lb2 ub2])
  )

(defn contains [lb1 ub1 lb2 ub2]
    (and (<= lb1 lb2) (>= ub1 ub2)))

(defn isfullycontainedrange
  [line]
  (let
   [[lb1 ub1 lb2 ub2] (parseranges line)
    isfullcontained (or (contains lb1 ub1 lb2 ub2) (contains lb2 ub2 lb1 ub1))]
    (if isfullcontained 1 0)))

(defn countfullycontainedranges
  [lines]
  (apply + (map isfullycontainedrange lines)))

(defn part1
  []
  (let [lines (utils/readlines "resources/day4/input.txt")]
    (countfullycontainedranges lines)))

(defn overlaps [lb1 ub1 lb2]
  (and (>= ub1 lb2) (<= lb1 lb2)))

(defn isoverlap
  [line]
  (let
   [[lb1 ub1 lb2 ub2] (parseranges line)
    isoverlap (or (overlaps lb1 ub1 lb2) (overlaps lb2 ub2 lb1))]
    (if isoverlap 1 0)))

(defn countoverlappingranges
  [lines]
  (apply + (map isoverlap lines)))

(defn part2
  []
  (let [lines (utils/readlines "resources/day4/input.txt")]
    (countoverlappingranges lines)))