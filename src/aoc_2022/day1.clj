(ns aoc-2022.day1 
  (:require [aoc-2022.utils :as utils]
            [clojure.string :refer [blank?]]))

(defn sumcals [lines]
  (map #(apply + %) (partition-by #(= 0 %) (map #(if (blank? %) 0 (Integer/parseInt %)) lines)))
  )

(defn part1 [] 
  (let [
        lines (utils/readlines "resources/day1/input.txt")
        sums (sumcals lines)
        ]
    (apply max sums)))

(defn part2 []
  (let [lines (utils/readlines "resources/day1/input.txt")
        sums (sumcals lines)]
    (apply + (take 3 (reverse (sort sums))))))