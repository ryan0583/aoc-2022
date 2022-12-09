(ns aoc-2022.day9
  (:require [aoc-2022.utils :as utils]))

(defn movehead [direction currenthead]
  (case direction
    "R" [(+ (first currenthead) 1) (second currenthead)]
    "L" [(- (first currenthead) 1) (second currenthead)]
    "U" [(first currenthead) (+ (second currenthead) 1)]
    "D" [(first currenthead) (- (second currenthead) 1)]))

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

(defn moverope [direction tailhistory currenthead]
  (let [
        newheadposition (movehead direction @currenthead)
        newtailposition (updatetail (last @tailhistory) newheadposition)
        ]
    (swap! currenthead (constantly newheadposition))
    (swap! tailhistory conj newtailposition)
    ))

(defn processline [line tailhistory currenthead]
  (let [
        direction (str (first line))
        movecount (Integer/parseInt (subs line 2))
        ]
    (doseq [_ (range movecount)]
            (moverope direction tailhistory currenthead))
    ))

(defn tailhistory [_] (atom [[0 0]]))

(defn process [lines]
  (let [
        tailhistory (tailhistory 0)
        currenthead (atom [0 0])
        ]
    (doseq [line lines]
      (processline line tailhistory currenthead)
      )
    (count (vec (set @tailhistory)))))

(defn part1 []
  (let
   [input (utils/readlines "resources/day9/input.txt")]
    (process input)
    ))

(defn move [direction tailhistories currenthead]
  (let [newheadposition (movehead direction @currenthead)]
    (swap! currenthead (constantly newheadposition))
    (swap! (last tailhistories) conj (updatetail (last @(last tailhistories)) @currenthead))
    (doseq [tailindex (reverse (range 8))]
      (swap! (nth tailhistories tailindex) conj (updatetail (last @(nth tailhistories tailindex)) 
                                                            (last @(nth tailhistories (+ tailindex 1))))))
    ))

(defn processlinept2 [line tailhistories currenthead]
  (let [direction (str (first line))
        movecount (Integer/parseInt (subs line 2))
        ]
    (doseq [_ (range movecount)]
      (move direction tailhistories currenthead))))

(defn processpt2 [lines]
  (let [tailhistories (vec (map tailhistory (range 9)))
        currenthead (atom [0 0])]
    (doseq [line lines]
      (processlinept2 line tailhistories currenthead))
    (count (vec (set @(first tailhistories))))))

(defn part2 []
  (let
   [input (utils/readlines "resources/day9/input.txt")]
    (processpt2 input)))