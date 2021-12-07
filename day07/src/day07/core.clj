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

(defn -main
  []
  (println positions))
