(ns aoc-2022.day5
  (:require [aoc-2022.utils :as utils :refer [transpose]]
            [clojure.string :as str :refer [blank?]]))

(defn prunestack [stack] (map #(str (second %)) stack))

(defn excludeblanks [stack] 
  (filter #(not (blank? %)) stack))

(defn parsestacks [lines]
  (let [
        stacklines (filter #(or (str/starts-with? % "[") (str/starts-with? % "  ")) lines)
        stacks (transpose (map #(partition 2 4 [" "] %) stacklines))
        stacks (map prunestack stacks)
        stacks (vec (map excludeblanks stacks))
        ]
    stacks
    )
  )

(defn parseintructions [lines] (filter #(str/starts-with? % "move") lines))

(defn parseinstruction [instruction] 
  (let [splitinstruction (str/split instruction #"\s")
        fromstacknumber (- (Integer/parseInt (nth splitinstruction 3)) 1)
        tostacknumber (- (Integer/parseInt (nth splitinstruction 5)) 1)
        numbertomove (Integer/parseInt (nth splitinstruction 1))]
    [fromstacknumber tostacknumber numbertomove]))

(defn fromstack [stacksatom fromstacknumber numbertomove]
  (let [oldfromstack (nth @stacksatom fromstacknumber)
        newfromstack (drop numbertomove oldfromstack)]
    [oldfromstack newfromstack]))

(defn newstacks [stacksatom fromstacknumber newfromstack tostacknumber newtostack] 
  (assoc (assoc @stacksatom fromstacknumber newfromstack) tostacknumber newtostack))

(defn performinstruction [stacksatom instruction]
  (let [
        [fromstacknumber tostacknumber numbertomove] (parseinstruction instruction)
        [oldfromstack newfromstack] (fromstack stacksatom fromstacknumber numbertomove)
        newtostack (concat (reverse (take numbertomove oldfromstack)) (nth @stacksatom tostacknumber))
        newstacks (newstacks stacksatom fromstacknumber newfromstack tostacknumber newtostack)
        ]
    (swap! stacksatom (constantly newstacks))
    ))

(defn performinstructions [stacksatom instructions] 
  (doseq [instruction instructions] (performinstruction stacksatom instruction))
  )


(defn parseinput [] 
  (let [lines        (utils/readlines "resources/day5/input.txt")
        stacks       (parsestacks lines)
        stacksatom   (atom stacks)
        instructions (parseintructions lines)]
    [stacksatom instructions]))

(defn part1 []
  (let 
   [[stacksatom instructions] (parseinput)]
    (performinstructions stacksatom instructions) 
    (apply str (map first @stacksatom))
    ))

(defn performinstructionpt2 [stacksatom instruction]
  (let [[fromstacknumber tostacknumber numbertomove] (parseinstruction instruction)
        [oldfromstack newfromstack] (fromstack stacksatom fromstacknumber numbertomove)
        newtostack (concat (take numbertomove oldfromstack) (nth @stacksatom tostacknumber))
        newstacks (newstacks stacksatom fromstacknumber newfromstack tostacknumber newtostack)]
    (swap! stacksatom (constantly newstacks))))

(defn performinstructionspt2 [stacksatom instructions]
  (doseq [instruction instructions] (performinstructionpt2 stacksatom instruction)))

(defn part2 []
  (let 
   [[stacksatom instructions] (parseinput)]
    (performinstructionspt2 stacksatom instructions)
    (apply str (map first @stacksatom))
    ))