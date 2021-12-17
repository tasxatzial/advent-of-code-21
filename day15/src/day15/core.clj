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

(defn process-adjacent
  "Updates the queue location cost when given a known from-cost."
  [queue visited loc from-cost x-or-y invalid-x-or-y]
  (if (or (= x-or-y invalid-x-or-y) (contains? visited loc))
    queue
    (let [new-cost (+ from-cost (get-in risk-levels loc))
          curr-cost (get queue loc)]
      (if (or (not curr-cost) (< new-cost curr-cost))
        (assoc queue loc new-cost)
        queue))))

(defn find-min-sum
  "Returns a map of [location -> total risk cost to location]. Start location is top-left."
  ([rows columns]
   (let [queue (priority-map [0 0] 0)]
     (find-min-sum queue {} rows columns)))
  ([queue visited rows columns]
   (loop [queue queue
          visited visited]
     (if-let [[[x y :as loc] from-cost] (first queue)]
       (let [new-queue (-> queue
                           (process-adjacent visited [(inc x) y] from-cost (inc x) rows)
                           (process-adjacent visited [x (inc y)] from-cost (inc y) columns)
                           (process-adjacent visited [x (dec y)] from-cost (dec y) -1)
                           (process-adjacent visited [(dec x) y] from-cost (dec x) -1)
                           (dissoc loc))
             new-visited (assoc visited loc from-cost)]
         (recur new-queue new-visited))
       visited))))

(defn -main
  []
  (println (find-min-sum rows columns)))
