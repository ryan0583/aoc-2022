(ns aoc-2022.day10 
  (:require [aoc-2022.utils :as utils]
            [clojure.string :refer [index-of]]))

(defn process-noop [instruction-index]
  (swap! instruction-index + 1))

(defn process-command [current-instruction new-instruction]
  (swap! current-instruction (constantly new-instruction)))

(defn read-instruction 
  [instruction-index current-instruction lines]
  (let [instruction     (nth lines @instruction-index)
        isnoop          (= "noop" instruction)
        new-instruction (when (not isnoop) 
                          (subs instruction 
                                (+ (index-of instruction " ") 1)))]
    (if isnoop
      (process-noop instruction-index)
      (process-command current-instruction (Integer/parseInt new-instruction))))
  )

(defn process-instruction [instruction-index current-instruction instruction-history]
  (swap! instruction-history conj @current-instruction)
  (swap! current-instruction (constantly nil))
  (swap! instruction-index + 1))

(defn perform-cycle [instruction-index current-instruction instruction-history lines]
  (if (nil? @current-instruction) 
    (read-instruction instruction-index current-instruction lines)
    (process-instruction instruction-index current-instruction instruction-history)))

(defn process [lines]
  (let [instruction-index   (atom 0)
        current-instruction (atom nil)
        instruction-history (atom [1])
        running-total       (atom [])]
    (doseq [cycle-number (range 221)]
      (when (.contains [20 60 100 140 180 220] (+ cycle-number 1))
        ;; (println @instruction-history)
        (swap! running-total conj (* (apply + @instruction-history) (+ cycle-number 1)))
        ;; (println (apply + @instruction-history))
        ;; (println (+ cycle-number 1))
        ;; (println @running-total)
        ;; (println)
        )
      (perform-cycle instruction-index current-instruction instruction-history lines) 
      )
    (apply + @running-total)))

(defn part1 []
  (let
   [input (utils/readlines "resources/day10/input.txt")]
    (process input)))

(defn part2 []
  (let
   [input (map-indexed vector (utils/readlines "resources/day10/input.txt"))]
    ""))