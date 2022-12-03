(ns aoc-2022.day3 
  (:require [aoc-2022.utils :as utils]
            [clojure.string :refer [includes?]]))

(defn charvalue 
  [char] 
  (let [asciival (int char)]
    (if (> asciival 90) 
      (- asciival 96) 
      (- asciival 38))))

(defn linepriority 
  [line]
  (let 
   [
    charcount (count line)
    halfcharcount (/ charcount 2)
    firstcompartment (subs line 0 halfcharcount)
    secondcompartment (subs line halfcharcount)
    commonchars (filter #(includes? secondcompartment (str %)) (seq firstcompartment))
    ]
    (charvalue (first commonchars))))

(defn totalpriority 
  [lines] 
  (apply + (map linepriority lines)))

(defn part1 
  [] 
  (let [lines (utils/readlines "resources/day3/input.txt")]
    (totalpriority lines)))

(defn groupbadgepriority 
  [group]
  (let [
        line1 (first group)
        line2 (second group)
        line3 (last group)
        commonchars (filter 
                     #(includes? line3 (str %)) 
                     (filter
                      #(includes? line2 (str %))
                      (seq line1)))
        ]
  (charvalue (first commonchars))))

(defn totalbadgepriority 
  [groupedlines]
  (apply + (map groupbadgepriority groupedlines)))

(defn part2
  []
  (let [
        lines (utils/readlines "resources/day3/input.txt")
        groupedlines (partition 3 lines)
        ]
    (totalbadgepriority groupedlines)))