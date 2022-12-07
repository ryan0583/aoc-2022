(ns aoc-2022.day7 
  (:require [aoc-2022.utils :as utils]
            [clojure.string :as str]))

(defrecord dir [parent name files dirs])

(defrecord file [name size])

(defn parsedir [line currentdir]
  (let [
        spaceindex (str/index-of line " ")
        dirname (subs line (+ spaceindex 1))
        newdir (dir. @currentdir dirname [] [])
        newdirs (conj (:dirs @currentdir) newdir)
        newcurrentdir (assoc @currentdir :dirs newdirs)
        ]
    (swap! currentdir (constantly newcurrentdir))
   ))

(defn parsefile [line currentdir]
  (let [
        spaceindex (str/index-of line " ")
        filesize (subs line 0 spaceindex)
        filename (subs line (+ spaceindex 1))
        file (file. filename filesize) 
        newfiles (conj (:files @currentdir) file)
        newcurrentdir (assoc @currentdir :files newfiles)
        ]
    (swap! currentdir (constantly newcurrentdir))
    )
  )

(defn parsefileordir 
  [line currentdir]
  (let [
        isdir (str/starts-with? line "dir")
        ]
    (if isdir
      (parsedir line currentdir) 
      (parsefile line currentdir)))
  )

(defn changedir [line currentdir]
  (let [
        newdirname (subs line 5)
        newdir     (if (= ".." newdirname) 
                     (:parent @currentdir)
                     (if (= "/" newdirname)
                       @currentdir
                       (some #(when (= newdirname (:name %)) %) (:dirs @currentdir))))
        ]
    (swap! currentdir (constantly newdir))))

(defn parseusercommand 
  [line currentdir] 
  (when 
   (str/starts-with? line "$ cd")
    (changedir line currentdir))
  )

(defn parseline 
  [line currentdir] 
  (if (str/starts-with? line "$") 
    (parseusercommand line currentdir) 
    (parsefileordir line currentdir)))

(defn process 
  [lines]
  (let [
        rootdir    (dir. nil "/" [] [])
        currentdir (atom rootdir)
        ]
    (doseq [line lines] (parseline line currentdir)) 
    (println @currentdir)
    ))

(defn part1 []
  (let
   [input (utils/readlines "resources/day7/input.txt")]
    (process input)))

(defn part2 []
  (let
   [input (utils/readlines "resources/day7/input.txt")]
    ))