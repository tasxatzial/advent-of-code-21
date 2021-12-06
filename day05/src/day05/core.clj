(ns day05.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-line
  "Parses a line (string) and returns a collection of 2 items, each item represents
  a (x,y) point."
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

(defn create-points1
  "Creates all points in a horizontal or vertical line segment."
  [line]
  (cond
    (horizontal? line) (create-horizontal-points line)
    (vertical? line) (create-vertical-points line)
    :else '()))

(defn update-points-map
  "Updates the points map with all the points of a line segment.
  If the map already contains a point, its count is increased by 1."
  [line points-map create-points-fn]
  (let [line-points (create-points-fn line)]
    (reduce (fn [result point]
              (if (get result point)
                (update result point inc)
                (assoc result point 1)))
            points-map line-points)))

(defn create-all-points
  "Creates the points map from a collection of line segments.
  Each key represents a [x y] point and its value counts the line segments
  that contain the point."
  [lines create-points-fn]
  (reduce #(update-points-map %2 %1 create-points-fn) {} lines))

(defn collect-at-least-two-overlaps
  "Collects all points that are contained in at least two line segments."
  [points-map]
  (filter #(> (second %) 1) points-map))

; --------------------------
; problem 2

(defn create-diagonal-points
  "Creates all points in a diagonal line segment."
  [[[x1 y1] [x2 y2]]]
  (let [minx (min x1 x2)
        maxx (max x1 x2)
        miny (min y1 y2)
        maxy (max y1 y2)]
    (if (or (and (= minx x1) (= miny y1))
            (and (= minx x2) (= miny y2)))
      (for [x (range minx (inc maxx))
            :let [y (+ miny (- x minx))]]
        [x y])
      (for [x (range minx (inc maxx))
            :let [y (- maxy (- x minx))]]
        [x y]))))

(defn create-points2
  "Creates all points in a line segment."
  [line]
  (cond
    (horizontal? line) (create-horizontal-points line)
    (vertical? line) (create-vertical-points line)
    :else (create-diagonal-points line)))

; --------------------------
; results

(defn day05-1
  []
  (-> vent-lines
      (create-all-points create-points1)
      collect-at-least-two-overlaps
      count))

(defn -main
  []
  (println (day05-1)))
