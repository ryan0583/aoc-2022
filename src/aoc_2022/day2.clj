(ns aoc-2022.day2 
  (:require [aoc-2022.utils :as utils]
            [clojure.string :refer [split]]))

(defn charscore [char] 
  (case char 
    "X" 1 
    "Y" 2 
    "Z" 3))

(defn isdraw? [firstchar secondchar]
  (or
   (and (= "A" firstchar) (= "X" secondchar))
   (and (= "B" firstchar) (= "Y" secondchar))
   (and (= "C" firstchar) (= "Z" secondchar))))

(defn iswinner? [firstchar secondchar] 
  (or 
   (and (= "A" firstchar) (= "Y" secondchar))
   (and (= "B" firstchar) (= "Z" secondchar))
   (and (= "C" firstchar) (= "X" secondchar)))
  )

(defn outcomescore [firstchar secondchar] 
  (cond
    (iswinner? firstchar secondchar) 6
    (isdraw? firstchar secondchar) 3
    :else 0))

(defn part1linescore [line] 
  (let 
   [
    chars        (split line #"\s+")
    firstchar    (first chars)
    secondchar   (second chars)
    charscore    (charscore secondchar)
    outcomescore (outcomescore firstchar secondchar)
    ]
    (+ charscore outcomescore)))

(defn part1 []
  (let [lines (utils/readlines "resources/day2/input.txt")]
    (apply + (map part1linescore lines))))


(defn desiredoutcomescore [outcomecode] 
  (case outcomecode
    "X" 0
    "Y" 3
    "Z" 6))

(defn lossscore [otherplayerchoice]
  (case otherplayerchoice
    "A" 3
    "B" 1
    "C" 2))

(defn drawscore [otherplayerchoice]
  (case otherplayerchoice
    "A" 1
    "B" 2
    "C" 3))

(defn winscore [otherplayerchoice]
  (case otherplayerchoice
    "A" 2
    "B" 3
    "C" 1))

(defn choicescore [otherplayerchoice outcomecode]
  (cond
    (= "X" outcomecode) (lossscore otherplayerchoice)
    (= "Y" outcomecode) (drawscore otherplayerchoice)
    (= "Z" outcomecode) (winscore otherplayerchoice)
    ))

(defn part2linescore [line]
  (let
   [chars               (split line #"\s+")
    firstchar           (first chars)
    secondchar          (second chars)
    desiredoutcomescore (desiredoutcomescore secondchar)
    choicescore (choicescore firstchar secondchar)
    ]
    (+ desiredoutcomescore choicescore)))

(defn part2 []
  (let [lines (utils/readlines "resources/day2/input.txt")]
    (apply + (map part2linescore lines))))