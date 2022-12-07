(ns aoc-2022.day7 
  (:require [aoc-2022.utils :as utils]
            [clojure.string :as str]))

(defn parsedir [line currentdir directorychildren]
  (let [
        newdirname (subs line 4)
        newdirectorychildren (assoc @directorychildren @currentdir (vec (conj (@directorychildren @currentdir) newdirname)))
        ]
    (swap! directorychildren (constantly newdirectorychildren))
   ))

(defn parsefile [line currentdir directoryfiles filesizes]
  (let [
        spaceindex (str/index-of line " ")
        filesize (subs line 0 spaceindex)
        filename (subs line (+ 1 spaceindex))
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
        newcurrentdir (if (= ".." newdirname) (last @dirhistory) newdirname)
        newdirhistory (if (= ".." newdirname) (vec (drop-last @dirhistory)) (conj @dirhistory @currentdir))
        ]
    (when (not (= ".." newdirname)) (swap! dirlist (constantly (conj @dirlist newdirname))))
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
        ;; log (println "DIRECTORY: ")
        ;; log (println dir)
        
        mydirectorychildren (directorychildren dir)
        ;; log (println mydirectorychildren)
        
        mydirectoryfiles (directoryfiles dir)
        ;; log (println mydirectoryfiles)
        
        mydirectoryfilesizes (map #(Integer/parseInt (filesizes %)) mydirectoryfiles)
        ;; log (println mydirectoryfilesizes)
        
        totalfilesize (apply + mydirectoryfilesizes)
        ;; log (println totalfilesize)
        
        mychilddirectorysizes (map #(getdirsize % directorychildren directoryfiles filesizes) mydirectorychildren)
        ;; log (println mychilddirectorysizes)
        
        totaldirectorysizes (apply + mychilddirectorysizes)
        ;; log (println totaldirectorysizes)
        
        ;; log (println (+ totalfilesize totaldirectorysizes))
        
        ;; log (println)
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
    (println @dirlist)
    ;; (println)
    ;; (println @dirhistory)
    ;; (println)
    ;; (println @currentdir)
    (println)
    (println @directorychildren)
    (println)
    (println @directoryfiles)
    (println)
    (println @filesizes)
    (apply + (filter #(< % 100000) (map #(getdirsize % @directorychildren @directoryfiles @filesizes) @dirlist)))
    ))

(defn part1 []
  (let
   [input (utils/readlines "resources/day7/input.txt")]
    (process input)))

(defn part2 []
  (let
   [input (utils/readlines "resources/day7/input.txt")]
    ))