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

(defn create-vertical-points
  "Creates all points in a vertical line segment."
  [[[x1 y1] [x2 y2]]]
  (let [min (min y1 y2)
        max (max y1 y2)]
    (for [y (range min (inc max))]
      [x1 y]))) []

(defn create-horizontal-points
  "Creates all points in a horizontal line segment."
  [[[x1 y1] [x2 y2]]]
  (let [min (min x1 x2)
        max (max x1 x2)]
    (for [x (range min (inc max))]
      [x y1])))

(defn create-points
  "Creates all points in a horizontal or vertical line segment."
  [line]
  (cond
    (horizontal? line) (create-horizontal-points line)
    (vertical? line) (create-vertical-points line)
    :else '()))

(defn -main
  []
  (println (collect-horizontal-vertical-lines)))