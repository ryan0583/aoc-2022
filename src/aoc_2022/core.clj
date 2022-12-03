(ns aoc-2022.core
  (:gen-class))


(defn getresult
  [ns]
  (let [
        part1 (ns-resolve ns 'part1)
        part2 (ns-resolve ns 'part2)
        ]
    (str "Part1: " (part1) ", Part2: " (part2)))
  )