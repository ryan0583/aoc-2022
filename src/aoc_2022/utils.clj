(ns aoc-2022.utils 
  (:require [clojure.string :as string]))

(defn readlines [filename]
  (string/split-lines (slurp filename)))


