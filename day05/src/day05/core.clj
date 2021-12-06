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

; --------------------------
; problem 1

(defn horizontal?
  "Returns true if the given segment is horizontal, false otherwise."
  [[[x1 y1] [x2 y2]]]
  (= y1 y2))

(defn vertical?
  "Returns true if the given segment is vertical, false otherwise."
  [[[x1 y1] [x2 y2]]]
  (= x1 x2))

(defn collect-horizontal-vertical-lines
  "Collects all horizontal or vertical lines."
  []
  (filter #(or (horizontal? %) (vertical? %)) vent-lines))

(defn -main
  []
  (println (collect-horizontal-vertical-lines)))
