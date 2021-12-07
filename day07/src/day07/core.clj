(ns day07.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Parses the input string and returns collection of numbers."
  [s]
  (let [split-lines (clojure.string/split-lines s)
        positions (first split-lines)]
    (map #(Integer/parseInt %) (clojure.string/split positions #","))))

(def positions (parse (slurp input-file)))

(defn total-fuel
  "Finds the total fuel when each crab moves to position x."
  [positions x fn_fuel]
  (->> positions
       (map #(fn_fuel % x))
       (apply +)))

(defn min-fuel
  "Finds the total fuel required when every crab moves to the position that
  minimizes the total fuel consumption."
  [positions fn_min-fuel-pos fn_fuel]
  (let [min-pos (fn_min-fuel-pos positions)]
    (if (int? min-pos)
      (total-fuel positions min-pos fn_fuel)
      (let [round-down (Math/round (Math/floor min-pos))]
        (min (total-fuel positions round-down fn_fuel)
             (total-fuel positions (inc round-down) fn_fuel))))))

; --------------------------
; problem 1

(defn median
  "Returns the median of collection of integer positions."
  [positions]
  (let [sorted (sort positions)
        length (count sorted)]
    (if (odd? length)
      (nth sorted (/ (dec length) 2))
      (/ (+ (nth sorted (/ length 2))
            (nth sorted (dec (/ length 2)))) 2))))

(defn find-min-fuel-pos1
  "Returns the position for which the fuel consumption is minimized (problem 1)."
  [positions]
  (median positions))

(defn calc-fuel1
  "Calculates the fuel required to move in position x from pos-start (problem 1)."
  [pos-start x]
  (Math/abs ^Integer (- pos-start x)))

; --------------------------
; problem 2

(defn mean
  "Returns the mean of collection of integer positions."
  [positions]
  (/ (apply + positions) (count positions)))

(defn find-min-fuel-pos2
  "Returns the position for which the fuel consumption is minimized (problem 2)."
  [positions]
  (mean positions))

; --------------------------
; results

(defn day07-1
  []
  (min-fuel positions find-min-fuel-pos1 calc-fuel1))

(defn -main
  []
  (println (day07-1)))
