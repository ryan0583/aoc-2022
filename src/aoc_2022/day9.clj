(ns aoc-2022.day9
  (:require [aoc-2022.utils :as utils]))

(defn istouching [tailposition headposition]
  (or (= tailposition headposition)
      
      ;; horizontal
      (= tailposition [(+ (first headposition) 1) (second headposition)])
      (= tailposition [(- (first headposition) 1) (second headposition)])
      
      ;; vectical
      (= tailposition [(first headposition) (+ (second headposition) 1)])
      (= tailposition [(first headposition) (- (second headposition) 1)])
      
      ;; diagonal right
      (= tailposition [(+ (first headposition) 1) (+ (second headposition) 1)])
      (= tailposition [(+ (first headposition) 1) (- (second headposition) 1)])
      
      ;; diagonal left
      (= tailposition [(- (first headposition) 1) (+ (second headposition) 1)])
      (= tailposition [(- (first headposition) 1) (- (second headposition) 1)])))

(defn movetail 
  [tailposition headposition]
  (cond
    ;; Diagonal
    (and (> (first headposition) (first tailposition)) (> (second headposition) (second tailposition))) [(+ (first tailposition) 1) (+ (second tailposition) 1)]
    (and (> (first headposition) (first tailposition)) (< (second headposition) (second tailposition))) [(+ (first tailposition) 1) (- (second tailposition) 1)]
    (and (< (first headposition) (first tailposition)) (< (second headposition) (second tailposition))) [(- (first tailposition) 1) (- (second tailposition) 1)]
    (and (< (first headposition) (first tailposition)) (> (second headposition) (second tailposition))) [(- (first tailposition) 1) (+ (second tailposition) 1)]
    
    ;; Horizontal
    (> (first headposition) (first tailposition)) [(+ (first tailposition) 1) (second tailposition)]
    (< (first headposition) (first tailposition)) [(- (first tailposition) 1) (second tailposition)]
    
    ;; Vertical
    (> (second headposition) (second tailposition)) [(first tailposition) (+ (second tailposition) 1)]
    (< (second headposition) (second tailposition)) [(first tailposition) (- (second tailposition) 1)]
    ))

(defn updatetail [tailposition headposition]
  (let [
        istouching (istouching tailposition headposition)
        newtailposition (if istouching tailposition (movetail tailposition headposition))
        ]
    newtailposition))

(defn movehead [direction tailhistory currenthead]
  (let [
        newheadposition (case direction
                          "R" [(+ (first @currenthead) 1) (second @currenthead)]
                          "L" [(- (first @currenthead) 1) (second @currenthead)]
                          "U" [(first @currenthead) (+ (second @currenthead) 1)]
                          "D" [(first @currenthead) (- (second @currenthead) 1)])
        ;; log (println newheadposition)
        newtailposition (updatetail (last @tailhistory) newheadposition)
        ;; log (println newtailposition)
        ;; log (println @tailhistory)
        ;; log (println)
        ]
    (swap! currenthead (constantly newheadposition))
    (swap! tailhistory conj newtailposition)
    ))

(defn processline [line tailhistory currenthead]
  (let [
        direction (str (first line))
        movecount (Integer/parseInt (subs line 2))
        ;; log (println movecount)
        ]
    (doseq [moveindex (range movecount)]
            (movehead direction tailhistory currenthead))
    ))

(defn process [lines]
  (let [
        tailhistory (atom [[0 0]])
        currenthead (atom [0 0])
        ]
    (doseq [line lines]
      (processline line tailhistory currenthead)
      )
    ;; (println @tailhistory)
    (count (vec (set @tailhistory)))))

(defn part1 []
  (let
   [input (utils/readlines "resources/day9/input.txt")]
    (process input)))

(defn part2 []
  (let
   [input (utils/readlines "resources/day9/input.txt")]
    ""))