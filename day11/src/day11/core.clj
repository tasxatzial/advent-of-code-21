(ns day11.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn char->int
  "Converts a char digit to the corresponding integer."
  [c]
  (- (int c) 48))

(defn parse
  "Parses the input string and returns a map, keys are the octopus positions
  (0 to row * column) and values are vectors describing their initial states
  [initial-energy flashed]. Flashed is true only when an octopus flashed in the
  current step."
  [s]
  (let [split-lines (clojure.string/split-lines s)
        rows (count split-lines)
        columns (count (first split-lines))
        octopus-states (map #(vector (char->int %) false) (apply str split-lines))
        octopus-positions (range (* rows columns))]
    [rows columns (zipmap octopus-positions octopus-states)]))

(def parsed-input (parse (slurp input-file)))
(def octopuses (get parsed-input 2))
(def rows (get parsed-input 0))
(def columns (get parsed-input 1))
(def octopuses-count (* rows columns))

(defn -main
  []
  (println octopuses))
