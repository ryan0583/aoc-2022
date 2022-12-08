(ns aoc-2022.day8
  (:require [aoc-2022.utils :as utils]))

(defn checkvertical [height x y lines]
  (let [otherheights (map #(Integer/parseInt (str (first (subs % x (+ x 1))))) lines)
        atedge (or (= y 0)
                   (= y (- (count otherheights) 1)))
        above (vec (take y otherheights))
        ;; log     (println (str "ABOVE: " above))
        below (vec (take-last (- (- (count otherheights) y) 1) otherheights))
        ;; log (println (str "BELOW: " below))
        ;; log (println (str "Y: " y))
        ;; log (println atedge)
        ]
    (or atedge
        (every? #(< % height) above)
        (every? #(< % height) below))))

(defn checkhorizontal [height x line]
  (let [atedge  (or (= x 0)
                    (= x (- (count line) 1)))
        toleft  (if (= x 0) "" (subs line 0 x))
        ;; log     (println (str "TO LEFT: " toleft))
        toright (subs line (+ 1 x))
        ;; log     (println (str "TO RIGHT: " toright))
        ;; log (println (str "X: " x))
        ;; log (println atedge)
        ]
    (or atedge 
        (every? #(< (Integer/parseInt (str %)) height) toleft) 
        (every? #(< (Integer/parseInt (str %)) height) toright))))

(defn isvisible [height x y line lines]
  (or (checkhorizontal height x line) (checkvertical height x y lines)))

(defn processline [y line lines count]
  (doseq [[x height] (vec (map-indexed vector line))]
    (when (isvisible (Integer/parseInt (str height)) x y line lines) (swap! count + 1))
    ;; (println (str "X: " x " Y: " y " HEIGHT: " height " VISIBLE: " (isvisible (Integer/parseInt (str height)) x y line lines)))
    ;; (println)
    )
  ;; (println (str "COUNT: " @count))
  ;; (println)
  )

(defn process
  [lines]
  (let [count (atom 0)]
    (doseq [[y line] (vec (map-indexed vector lines))]
      (processline y line lines count))
    @count))

(defn part1 []
  (let
   [input (utils/readlines "resources/day8/input.txt")]
    (process input)))

(defn part2 []
  (let
   [input (utils/readlines "resources/day8/input.txt")]))