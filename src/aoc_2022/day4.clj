(ns aoc-2022.day4 
  (:require [aoc-2022.utils :as utils]
            [clojure.string :refer [split]]))

(defn contains [outer inner]
  (let [
        outersplit (split outer #"-")
        innersplit (split inner #"-")
        outerlb (Integer/parseInt (first outersplit))
        outerub (Integer/parseInt (second outersplit))
        innerlb (Integer/parseInt (first innersplit))
        innerub (Integer/parseInt (second innersplit))]
    (and (<= outerlb innerlb) (>= outerub innerub)))
  )

(defn isfullycontainedrange 
  [line]
  (let 
   [
    split (split line #",")
    firstrange (first split)
    secondrange (second split) 
    isfullcontained (or (contains firstrange secondrange) (contains secondrange firstrange))
    ]
   (if isfullcontained 1 0))
  )

(defn countfullycontainedranges 
  [lines]
  (apply + (map isfullycontainedrange lines))
  )

(defn part1
  []
  (let [lines (utils/readlines "resources/day4/input.txt")]
    (countfullycontainedranges lines)))

(defn overlaps [range1 range2]
  (let [range1split (split range1 #"-") 
        range2split (split range2 #"-")
        range1lb (Integer/parseInt (first range1split))
        range1ub (Integer/parseInt (second range1split))
        range2lb (Integer/parseInt (first range2split))]
    (and (>= range1ub range2lb) (<= range1lb range2lb))))

(defn isoverlap
  [line]
  (let
   [split (split line #",")
    firstrange (first split)
    secondrange (second split)
    isoverlap (or (overlaps firstrange secondrange) (overlaps secondrange firstrange))]
    (if isoverlap 1 0)))

(defn countoverlappingranges
  [lines]
  (apply + (map isoverlap lines)))

(defn part2
  []
  (let [lines (utils/readlines "resources/day4/input.txt")]
    (countoverlappingranges lines)))