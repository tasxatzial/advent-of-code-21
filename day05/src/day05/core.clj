(ns day05.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-line
  "Parses a line (string) and returns a collection of 2 items, each item represents
  a (x,y) coordinate."
  [s]
  (let [split-line (clojure.string/split s #" -> |,")]
    (->> split-line
         (map #(Integer/parseInt %))
         (partition 2))))

(defn parse
  "Parses the input string and returns a collection of line segments. Each segment
  is represented by the result of parse-line."
  [s]
  (->> s
       clojure.string/split-lines
       (map parse-line)))

(def vent-lines (parse (slurp input-file)))

(defn -main
  []
  (println vent-lines))
