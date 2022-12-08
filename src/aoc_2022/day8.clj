(ns aoc-2022.day8
  (:require [aoc-2022.utils :as utils]))

(defn maxindex [heights] (- (count heights) 1))

(defn columnheights [index lines] (vec (map #(get % index) lines)))

(defn before [index heights] (vec (take index heights)))

(defn after [index heights] (vec (take-last (- (maxindex heights) index) heights)))

(defn check [index height heights]
  (let [
        before (before index heights)
        after (after index heights)]
    (or (every? #(< % height) before)
        (every? #(< % height) after))))

(defn checkvertical [height x y lines]
  (check y height (columnheights x lines)))

(defn checkhorizontal [height x line]
  (check x height line))

(defn isvisible [height x y line lines]
  (or (checkhorizontal height x line) (checkvertical height x y lines)))

(defn processline [y line lines]
  (let [indexedline (vec (map-indexed vector line))
        visiblecount (count (filter #(isvisible (second %) (first %) y line lines) indexedline))]
    visiblecount))

(defn converttoints [line] (vec (map #(Integer/parseInt (str %)) line)))

(defn process
  [lines]
  (let [indexedlines (vec (map-indexed vector lines))]
    (apply + (map #(processline (first %) (second %) lines) indexedlines))))

(defn part1 []
  (let
   [input (vec (map converttoints (utils/readlines "resources/day8/input.txt")))]
    (process input)))

(defn nullsafe [index default] (if (nil? index) default index))

(defn leftorabovescore
  [index height trees]
  (let [seenindex (first (last (filter #(>= (second %) height) trees)))
        score (- index (nullsafe seenindex 0))]
    score))

(defn rightorbelowscore
  [index height trees max]
  (let [seenindex (first (first (filter #(>= (second %) height) trees)))
        score (- (nullsafe seenindex max) index)]
    score))

(defn scenicscore
  [x height y indexedline lines]
  (let [columnheights (vec (map-indexed vector (columnheights x lines)))
        toleft  (before x indexedline)
        toright (after x indexedline)
        above  (before y columnheights)
        below (after y columnheights)
        
        leftscore (leftorabovescore x height toleft)
        rightscore (rightorbelowscore x height toright (maxindex indexedline))
        abovescore (leftorabovescore y height above)
        belowscore (rightorbelowscore y height below (maxindex columnheights))]
    (apply * [leftscore rightscore abovescore belowscore])))

(defn processlinept2 [y line lines]
  (let [indexedline (vec (map-indexed vector line))]
    (map #(scenicscore (first %) (second %) y indexedline lines) indexedline)))

(defn processpt2
  [lines]
  (let [indexedlines (vec (map-indexed vector lines))]
    (apply max (flatten (map #(processlinept2 (first %) (second %) lines) indexedlines)))))

(defn part2 []
  (let
   [input (vec (map converttoints (utils/readlines "resources/day8/input.txt")))]
    (processpt2 input)))