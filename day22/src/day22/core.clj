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

(defn -main
  []
  (println instructions))