(ns aoc-2022.day7 
  (:require [aoc-2022.utils :as utils]
            [clojure.string :as str]))

(defn parsedir [line currentdir directorychildren]
  (let [
        newdirname (str @currentdir "." (subs line 4))
        newdirectorychildren (assoc @directorychildren @currentdir (vec (conj (@directorychildren @currentdir) newdirname)))
        ]
    (swap! directorychildren (constantly newdirectorychildren))
   ))

(defn parsefile [line currentdir directoryfiles filesizes]
  (let [
        spaceindex (str/index-of line " ")
        filesize (subs line 0 spaceindex)
        filename (str @currentdir "." (subs line (+ 1 spaceindex)))
        newdirectoryfiles (assoc @directoryfiles @currentdir (vec (conj (@directoryfiles @currentdir) filename)))
        newfilesizes (assoc @filesizes filename filesize)
        ]
    (swap! directoryfiles (constantly newdirectoryfiles))
    (swap! filesizes (constantly newfilesizes))
    )
  )

(defn parsefileordir 
  [line currentdir directorychildren directoryfiles filesizes]
  (let [
        isdir (str/starts-with? line "dir")
        ]
    (if isdir (parsedir line currentdir directorychildren) (parsefile line currentdir directoryfiles filesizes))
  ))

(defn changedir [line dirlist dirhistory currentdir]
  (let [
        newdirname (subs line 5)
        dirnamewithparent (str @currentdir "." newdirname)
        newcurrentdir (if (= ".." newdirname) (last @dirhistory) dirnamewithparent)
        newdirhistory (if (= ".." newdirname) (vec (drop-last @dirhistory)) (conj @dirhistory @currentdir))
        ]
    (when (not (= ".." newdirname)) (swap! dirlist conj @dirlist dirnamewithparent))
    (swap! currentdir (constantly newcurrentdir))
    (swap! dirhistory (constantly newdirhistory))
    ))

(defn parseusercommand 
  [line dirlist dirhistory currentdir] 
  (when 
   (str/starts-with? line "$ cd")
    (changedir line dirlist dirhistory currentdir))
  )

(defn parseline 
  [line dirlist dirhistory currentdir directorychildren directoryfiles filesizes] 
  (if (str/starts-with? line "$") 
    (parseusercommand line dirlist dirhistory currentdir) 
    (parsefileordir line currentdir directorychildren directoryfiles filesizes)))

(defn getdirsize [dir directorychildren directoryfiles filesizes]
  (let [
        mydirectorychildren (directorychildren dir)
        mydirectoryfiles (directoryfiles dir)
        mydirectoryfilesizes (map #(Integer/parseInt (filesizes %)) mydirectoryfiles)
        totalfilesize (apply + mydirectoryfilesizes)
        mychilddirectorysizes (map #(getdirsize % directorychildren directoryfiles filesizes) mydirectorychildren)
        totaldirectorysizes (apply + mychilddirectorysizes)
        ]
    (+ totalfilesize totaldirectorysizes)
    )
  )

(defn process 
  [lines]
  (let [
        dirlist (atom [])
        dirhistory (atom [])
        currentdir (atom "START")
        directorychildren (atom {})
        directoryfiles (atom {})
        filesizes (atom {})
        ]
    (doseq [line lines] (parseline line dirlist dirhistory currentdir directorychildren directoryfiles filesizes))
    (map #(getdirsize % @directorychildren @directoryfiles @filesizes) @dirlist)
    ))

(defn getsmallestdirsize [lines]
  (
   let [dirsizes    (process lines)
        totalused   (apply max dirsizes)
        unusedspace (- 70000000 totalused)
        spaceneeded (- 30000000 unusedspace)
        ]
    (first (sort (filter #(> % spaceneeded) dirsizes)))))

(defn part1 []
  (let
   [input (utils/readlines "resources/day7/input.txt")]
    (apply + (filter #(< % 100000) (process input)))))

(defn part2 []
  (let
   [input (utils/readlines "resources/day7/input.txt")]
    (getsmallestdirsize input)
    ))