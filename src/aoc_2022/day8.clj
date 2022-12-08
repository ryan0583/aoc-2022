(ns aoc-2022.day8 
  (:require [aoc-2022.utils :as utils]))

(defn checkvertical [height x y lines]
  (let [atedge  (or (= y 0)
                    (= y (- (count lines) 1)))
        columnheights (vec (map #(get % x) lines))
        above  (if (= x 0) [] (vec (take y columnheights)))
        below (vec (take-last (- (- (count columnheights) y) 1) columnheights))
        ]
    (or atedge
        (every? #(< % height) above)
        (every? #(< % height) below))))

(defn checkhorizontal [height x line]
  (let [
        atedge  (or (= x 0)
                    (= x (- (count line) 1)))
        toleft  (if (= x 0) [] (vec (take x line)))
        toright (vec (take-last (- (- (count line) x) 1) line))
        ]
    (or atedge 
        (every? #(< % height) toleft) 
        (every? #(< % height) toright))))

(defn isvisible [height x y line lines]
  (or (checkhorizontal height x line) (checkvertical height x y lines)))

(defn processline [y line lines]
  (
   let [
        indexedline (vec (map-indexed vector line))
        visiblecount (count (filter #(isvisible (second %) (first %) y line lines) indexedline))
        ]
   visiblecount
   )
  )

(defn converttoints [line] (vec (map #(Integer/parseInt (str %)) line)))

(defn process
  [lines]
  (let [
        indexedlines (vec (map-indexed vector lines))
        ]
    (apply + (map #(processline (first %) (second %) lines) indexedlines))))

(defn part1 []
  (let
   [input (vec (map converttoints (utils/readlines "resources/day8/input.txt")))]
    (process input)))

(defn part2 []
  (let
   [input (utils/readlines "resources/day8/input.txt")]))