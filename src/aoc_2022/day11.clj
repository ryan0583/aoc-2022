(ns aoc-2022.day11
  (:require [aoc-2022.utils :as utils]
            [clojure.string :refer [split starts-with?]]))

(defn new-monkey [monkey-lines monkey-index]
  (swap! monkey-lines conj {:items        []
                            :operation-fn {:operator "" :value ""}
                            :test-fn      {:divisor 0 :true-monkey 0 :false-monkey 0}})
  (swap! monkey-index + 1)
  (println))

(defn process-items [command monkey-lines monkey-index]
  (let [items (vec (map #(Integer/parseInt %) (split (subs command 16) #",\s")))
        new-monkey-lines (assoc @monkey-lines monkey-index
                                (assoc (nth @monkey-lines monkey-index) :items items))]
    (swap! monkey-lines (constantly new-monkey-lines))))

(defn process-operarion [command monkey-lines monkey-index]
  (let [calc (subs command 11)
        operator-and-value (split (subs calc 10) #"\s")
        operator (first operator-and-value)
        value (second operator-and-value)
        new-monkey-lines (assoc @monkey-lines monkey-index
                                (assoc (nth @monkey-lines monkey-index) :operation-fn {:operator operator :value value}))]
    (swap! monkey-lines (constantly new-monkey-lines))))

(defn process-test [lines command monkey-lines monkey-index line-index]
  (let [divisor (Integer/parseInt (subs command 19))
        true-monkey (Integer/parseInt (str (last (second (nth lines (+ line-index 1))))))
        false-monkey (Integer/parseInt (str (last (second (nth lines (+ line-index 2))))))
        new-monkey-lines (assoc @monkey-lines monkey-index
                                (assoc (nth @monkey-lines monkey-index) 
                                       :test-fn {:divisor      divisor 
                                                 :true-monkey  true-monkey 
                                                 :false-monkey false-monkey}))]
    (swap! monkey-lines (constantly new-monkey-lines))))

(defn process-line [lines line monkey-lines monkey-index line-index]
  (when (not (= "" line))
    (let [command (subs line 2)]
      (cond
        (starts-with? command "Starting items:") (process-items command monkey-lines @monkey-index)
        (starts-with? command "Operation:") (process-operarion command monkey-lines @monkey-index)
        (starts-with? command "Test:") (process-test lines command monkey-lines @monkey-index line-index)
        :else "SKIP!"))))

(defn readline [lines line monkey-lines monkey-index line-index]
  (if (starts-with? line "Monkey")
    (new-monkey monkey-lines monkey-index)
    (process-line lines line monkey-lines monkey-index line-index)))

(defn process [lines]
  (let
   [monkey-lines (atom [])
    monkey-index (atom -1)]
    (doseq [[index line] lines]
      (readline lines line monkey-lines monkey-index index))
    (println)
    @monkey-lines))

(defn part1 []
  (let
   [input (map-indexed vector (utils/readlines "resources/day11/input.txt"))]
    (process input)))

(defn part2 []
  (let
   [input (utils/readlines "resources/day11/input.txt")]
    ""))