(ns day09.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-line
  "Parses an input line and returns a vector of numbers."
  [s]
  (mapv #(- (int %) 48) s))

(defn parse
  "Parses the input string and returns a collection of vectors of numbers."
  [s]
  (let [split-lines (clojure.string/split-lines s)
        rows (count split-lines)
        columns (count (first split-lines))
        heightmap (vec (flatten (map parse-line split-lines)))]
    [rows columns heightmap]))

(def parsed-input (parse (slurp input-file)))

; a vector of values, stored by rows
(def heightmap (get parsed-input 2))

(def rows (get parsed-input 0))
(def columns (get parsed-input 1))

(defn get-adjacent
  "Returns a collection of the adjacent locations to loc."
  [loc locations]
  (let [top (get locations (- loc columns))
        bottom (get locations (+ loc columns))
        left (when (pos? (mod loc columns))
               (get locations (dec loc)))
        right (when (not= (dec columns) (mod loc columns))
                (get locations (inc loc)))]
    (remove nil? [top bottom left right])))

(defn heightmap-locations
  "Generates a vector of locations (from 0 to rows * columns -1)."
  []
  (vec (range (* rows columns))))

(defn low-loc?
  "Returns true if loc is a low point, false otherwise."
  [loc locations]
  (let [val (get heightmap loc)
        adjacent-loc (get-adjacent loc locations)
        higher-adjacent (filter #(> (get heightmap %) val) adjacent-loc)]
    (= (count adjacent-loc) (count higher-adjacent))))

(defn risk-level
  "Returns the risk level of loc."
  [loc]
  (inc (get heightmap loc)))

; --------------------------
; problem 1

(defn total-risk-level
  "Returns the sum of the risk levels of all low points."
  []
  (let [locations (heightmap-locations)]
    (reduce (fn [result loc]
              (if (low-loc? loc locations)
                (+ result (risk-level loc))
                result))
            0 locations)))

; --------------------------
; results

(defn day09-1
  []
  (total-risk-level))

(defn -main
  []
  (println (day09-1)))
