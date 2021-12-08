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

(defn -main
  []
  (println signals))
