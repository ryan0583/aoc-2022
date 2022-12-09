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

(defn knotposition 
  ([] (knotposition 0)) 
  ([_] (atom [0 0])))

(defn tailhistory [] (atom [[0 0]]))

(defn process [lines]
  (let [
        tailhistory (tailhistory)
        currenthead (knotposition)
        ]
    (println "PART 1")
    (doseq [[index line] lines]
      (println index)
      (processline line tailhistory currenthead)
      )
    (println "DONE")
    (println)
    (count (vec (set @tailhistory)))))

(defn part1 []
  (let
   [input (vec (map-indexed vector (utils/readlines "resources/day9/input.txt")))]
    (process input)
    ))

(defn move [direction knotpositions tailhistory currenthead]
  (let [newheadposition (movehead direction @currenthead)]
    (swap! currenthead (constantly newheadposition))
    (swap! (last knotpositions) (constantly (updatetail @(last knotpositions) @currenthead)))
    (doseq [tailindex (reverse (range 8))]
      (swap! (nth knotpositions tailindex) 
             (constantly (updatetail
                          @(nth knotpositions tailindex)
                          @(nth knotpositions (+ tailindex 1))))))
    (swap! tailhistory conj @(first knotpositions))
    ))

(defn processlinept2 [line knotpositions tailhistory currenthead]
  (let [direction (str (first line))
        movecount (Integer/parseInt (subs line 2))
        ]
    (doseq [_ (range movecount)]
      (move direction knotpositions tailhistory currenthead))))

(defn processpt2 [lines]
  (let [knotpositions (vec (map knotposition (range 9)))
        tailhistory (tailhistory)
        currenthead (knotposition)]
    (println "PART 2")
    (doseq [[index line] lines]
      (println index)
      (processlinept2 line knotpositions tailhistory currenthead))
    (println "DONE")
    (println)
    (count (vec (set @tailhistory)))))

(defn part2 []
  (let
   [input (vec (map-indexed vector (utils/readlines "resources/day9/input.txt")))]
    (processpt2 input)))