(ns day22.core
  (:gen-class)
  (:require [clojure.set :refer [difference union]]))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-range
  "Parses a range (string) that has the form x=A..B and returns
  the vector [A B]."
  [s]
  (let [[_ left right] (clojure.string/split s #"=|\.\.")]
    (mapv #(Integer/parseInt %) [left right])))

(defn parse-line
  "Parses an input line (string) and returns an appropriate structure
  representing an instruction. Each instruction is a vector consisting of
  4 items. First item is :on or :off and the rest of the items are the
  x,y,z ranges, each range is described by a vector."
  [s]
  (let [[command cubes] (clojure.string/split s #" ")
        ranges (clojure.string/split cubes #",")
        parsed-ranges (map parse-range ranges)]
    (into [(keyword command)] parsed-ranges)))

(defn parse
  "Parses the input string and returns a collection of instructions.
  Each instruction is a vector as returned by parse-line."
  [s]
  (let [split-lines (clojure.string/split-lines s)]
    (map parse-line split-lines)))

(def instructions (parse (slurp input-file)))

; --------------------------
; problem 1

(defn less-than-50?
  "Returns true if for every range, the left number is >= -50
  and the right number is <= 50."
  ([[left right]]
   (and (>= left -50) (<= right 50)))
  ([x-range y-range z-range]
   (and (less-than-50? x-range)
        (less-than-50? y-range)
        (less-than-50? z-range))))

(defn create-cube
  "Returns all cubes that are part of the cuboid specified by the given
  x,y,z ranges."
  [[left-x right-x] [left-y right-y] [left-z right-z]]
  (for [x (range left-x (inc right-x))
        y (range left-y (inc right-y))
        z (range left-z (inc right-z))]
    [x y z]))

(defn apply-instructions1
  "Applies the instructions (problem 1). Returns a set of the cubes that are
  on at the end of the process."
  []
  (let [p1-instructions (filter #(less-than-50? (get % 1) (get % 2) (get % 3))
                                instructions)]
    (reduce (fn [result [command x-range y-range z-range]]
              (let [cube (set (create-cube x-range y-range z-range))]
                (if (= :on command)
                  (union result cube)
                  (difference result cube))))
            #{} p1-instructions)))

; --------------------------
; results

(defn day22-1
  []
  (count (apply-instructions1)))

(defn -main
  []
  (println (day22-1)))
