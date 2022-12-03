(ns aoc-2022.day2 
  (:require [aoc-2022.utils :as utils]
            [clojure.string :refer [split]]))

(defn charscore [char] 
  (case char 
    ("A" "X") 1 
    ("B" "Y") 2 
    ("C" "Z") 3))

(defn normalise [number]
  (let [normalised (if (< number 1) 3 number)
        normalised (if (> normalised 3) 1 normalised)]
    normalised))

(defn increment [number] (normalise (+ number 1)))

(defn decrement [number] (normalise (- number 1)))

(defn isdraw? [firstchar secondchar]
  (= (charscore firstchar) (charscore secondchar)))

(defn iswinner? [firstchar secondchar]
  (= (increment (charscore firstchar)) (charscore secondchar)))

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

(defn loss [outcomecode] (= "X" outcomecode))

(defn draw [outcomecode] (= "Y" outcomecode))

(defn win [outcomecode] (= "Z" outcomecode))

(defn desiredoutcomescore [outcomecode] 
  (cond
    (loss outcomecode) 0
    (draw outcomecode) 3 
    (win outcomecode) 6))

(defn choicescore [otherplayerchoice outcomecode]
  (let [otherplayerscore (charscore otherplayerchoice)]
    (cond
      (loss outcomecode) (decrement otherplayerscore)
      (draw outcomecode) otherplayerscore
      (win outcomecode) (increment otherplayerscore)
    )
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