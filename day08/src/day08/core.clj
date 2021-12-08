(ns day08.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-line
  "Parses an input line and returns a collection of two items. The first item is
  a vector of the signal patterns. The second item is a vector of the output values."
  [s]
  (let [split-line (clojure.string/split s #" [|] ")]
    (map #(clojure.string/split % #" ") split-line)))

(defn parse
  "Parses the input string into a collection that contains the results
  of parse-line when applied to each input line."
  [s]
  (let [split-input (clojure.string/split-lines s)]
    (map parse-line split-input)))

(def signals (parse (slurp input-file)))

(def digit->segment-count
  {0 6
   1 2
   2 5
   3 5
   4 4
   5 5
   6 6
   7 3
   8 7
   9 6})

; --------------------------
; problem 1

(def output-values (map second signals))

(defn count-digits-with-unique-segments
  "Takes as input a collection of 4 digits and counts how many times 1 4 7 8 appear in total."
  [output-value]
  (let [unique-segments (set (map digit->segment-count [1 4 7 8]))]
    (count (filter unique-segments (map count output-value)))))

(defn total-digits-with-unique-segments
  "Finds how many times digits 1 4 7 8 appear in the output values."
  []
  (apply + (map count-digits-with-unique-segments output-values)))

; --------------------------
; results

(defn day08-1
  []
  (total-digits-with-unique-segments))

(defn -main
  []
  (println (day08-1)))
