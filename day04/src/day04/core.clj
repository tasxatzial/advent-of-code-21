(ns day04.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-draw-numbers
  "Parses the string that represents the numbers drawn and returns
  a vector of numbers."
  [s]
  (let [draw-numbers (clojure.string/split s #",")]
    (mapv #(Integer/parseInt %) draw-numbers)))

(defn create-board
  "Creates a bingo board from a collection of 25 numbers (one row every 5 numbers).
  Returns a vector that contains
  10 sets of 5 numbers each. Each set represents a bingo row or column."
  [coll]
  (let [rows (mapv set (partition 5 coll))
        cols (->> coll
                  (zipmap (range 0 (count coll)))
                  (group-by #(mod (first %) 5))
                  vals
                  (map #(map second %))
                  (mapv #(set %)))]
    (into rows cols)))

(defn parse-boards
  "Parses a collection of lines (strings) and returns an appropriate structure that represents
  all bingo boards. The lines are all the lines in the input file except the first one.
  Returns a vector containing vectors. Each vector is the return value of create-board."
  [coll]
  (->> coll
       (filter seq)
       (map #(clojure.string/split % #" "))
       (map #(filter seq %))
       flatten
       (map #(Integer/parseInt %))
       (partition 25)
       (mapv create-board)))

(defn parse
  "Splits the input string by newline and creates the structures that represent the drawn
  numbers and the bingo boards. Returns a vector of two items. First one is the draw-numbers
  as returned by parse-draw-numbers. Second one is the bingo boards as returned by parse-boards."
  [s]
  (let [split-s (clojure.string/split-lines s)
        draw-numbers (parse-draw-numbers (first split-s))
        boards (parse-boards (rest split-s))]
    [draw-numbers boards]))

(def starting-state (parse (slurp input-file)))
(def draw-numbers (first starting-state))
(def boards (second starting-state))

(defn -main
  []
  (println draw-numbers))
