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

(defn -main
  []
  (println (create-board [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25])))
