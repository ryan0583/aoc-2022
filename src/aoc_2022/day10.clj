(ns aoc-2022.day10
  (:require [aoc-2022.utils :as utils]
            [clojure.string :refer [index-of]]))

(defn process-noop! [instruction-index]
  (swap! instruction-index + 1))

(defn process-command! [current-instruction new-instruction]
  (swap! current-instruction (constantly new-instruction)))

(defn read-instruction!
  [instruction-index current-instruction lines]
  (let [instruction     (nth lines @instruction-index)
        isnoop          (= "noop" instruction)
        new-instruction (when (not isnoop)
                          (subs instruction
                                (+ (index-of instruction " ") 1)))]
    (if isnoop
      (process-noop! instruction-index)
      (process-command! current-instruction (Integer/parseInt new-instruction)))))

(defn process-instruction! [instruction-index current-instruction instruction-history]
  (swap! instruction-history conj @current-instruction)
  (swap! current-instruction (constantly nil))
  (swap! instruction-index + 1))

(defn perform-cycle! [instruction-index current-instruction instruction-history lines]
  (if (nil? @current-instruction)
    (read-instruction! instruction-index current-instruction lines)
    (process-instruction! instruction-index current-instruction instruction-history)))

(defn x-register [instruction-history] (apply + instruction-history))

(defn process [lines]
  (let [instruction-index   (atom 0)
        current-instruction (atom nil)
        instruction-history (atom [1])
        running-total       (atom [])]
    (doseq [cycle-number (range 221)]
      (when (.contains [20 60 100 140 180 220] (+ cycle-number 1))
        (swap! running-total conj (* (x-register @instruction-history) (+ cycle-number 1))))
      (perform-cycle! instruction-index current-instruction instruction-history lines))
    (apply + @running-total)))

(defn part1 []
  (let
   [input (utils/readlines "resources/day10/input.txt")]
    (process input)))

(defn can-see-sprite? [column-index sprite-middle] 
  (<= (- column-index 1) sprite-middle (+ column-index 1)))

(defn update-row! [crt-row column-index sprite-middle] 
  (swap! crt-row str (if (can-see-sprite? column-index sprite-middle) "#" ".")))

(defn new-line! [crt-row column-index]
  (swap! crt-row (constantly ""))
  (swap! column-index (constantly 0)))

(defn print-row! [crt-row column-index] 
  (when (= 40 @column-index)
    (println @crt-row)
    (new-line! crt-row column-index)))

(defn processpt2 [lines]
  (let [instruction-index   (atom 0)
        current-instruction (atom nil)
        instruction-history (atom [1])
        crt-row (atom "")
        column-index (atom 0)]
    (doseq [_ (range 240)]
        (update-row! crt-row @column-index (x-register @instruction-history))
        (perform-cycle! instruction-index current-instruction instruction-history lines)
        (swap! column-index + 1)
        (print-row! crt-row column-index))))

(defn part2 []
  (let
   [input (utils/readlines "resources/day10/input.txt")]
    (processpt2 input)
    "SEE ABOVE!"))