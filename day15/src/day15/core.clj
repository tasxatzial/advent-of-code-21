(ns day15.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn char->int
  "Converts a digit char to the corresponding integer digit."
  [c]
  (- (int c) 48))

(defn parse
  "Parses the input string and returns a vector of vectors of integers.
  Each vector corresponds to a line in the input file."
  [s]
  (->> s
       clojure.string/split-lines
       (mapv #(mapv char->int %))))

(def risk-levels (parse (slurp input-file)))
(def rows (count risk-levels))
(def columns (count (first risk-levels)))

(defn -main
  []
  (println risk-levels))
