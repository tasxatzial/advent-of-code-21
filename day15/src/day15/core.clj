(ns day15.core
  (:gen-class)
  (:require [clojure.data.priority-map :refer [priority-map]]))

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

(defn get-risk
  "Returns the risk at the given location [x y]."
  [[x y]]
  (let [risk (get-in risk-levels [(rem x rows) (rem y columns)])
        new-risk (+ risk (quot x rows) (quot y columns))]
    (if (>= new-risk 10)
      (- new-risk 9)
      new-risk)))

(defn invalid-loc?
  "Returns true if both x and y are within bounds, false otherwise."
  [[x y] rows columns]
  (or (< x 0) (< y 0) (>= x rows) (>= y columns)))

(defn get-adjacent
  "Returns a vector of the 4 adjacent locations to [x y]."
  [[x y]]
  [[(inc x) y] [x (inc y)] [x (dec y)] [(dec x) y]])

(defn process-adjacent
  "Updates the cost of the adjacent locations to loc when given a known cost to loc
  and enqueues the adjacent loc (unless visited already contains the adjacent loc,
  in that case it won't be enqueued and its cost won't change).
  Visited: map of {loc -> min total cost to loc}."
  [queue loc cost visited rows columns]
  (reduce (fn [result adj-loc]
            (if (or (invalid-loc? adj-loc rows columns) (contains? visited adj-loc))
              result
              (let [new-cost (+ cost (get-risk adj-loc))
                    curr-cost (get result adj-loc)]
                (if (or (not curr-cost) (< new-cost curr-cost))
                  (assoc result adj-loc new-cost)
                  result))))
          queue (get-adjacent loc)))

(defn find-min-risks
  "Returns a map of {location -> total risk to location}. Start location is top-left."
  ([rows columns]
   (let [queue (priority-map [0 0] 0)]
     (find-min-risks queue {} rows columns)))
  ([queue visited rows columns]
   (loop [queue queue
          visited visited]
     (if-let [[loc cost] (first queue)]
       (let [new-queue (-> queue
                           (process-adjacent loc cost visited rows columns)
                           (dissoc loc))
             new-visited (assoc visited loc cost)]
         (recur new-queue new-visited))
       visited))))

; --------------------------
; results

(defn day15
  [rows columns]
  (let [min-risks (find-min-risks rows columns)]
    (get min-risks [(dec rows) (dec columns)])))

(defn -main
  []
  (println (day15 rows columns))
  (println (day15 (* rows 5) (* columns 5))))
