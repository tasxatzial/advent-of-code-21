(ns day06.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Parses the input string and returns collection of numbers."
  [s]
  (let [split-lines (clojure.string/split-lines s)
        ages (first split-lines)]
    (mapv #(Integer/parseInt %) (clojure.string/split ages #","))))

(def lanternfish (parse (slurp input-file)))

(defn init-lanternfish
  "Returns a vector, each index has the number of lanternfish that have age = index."
  []
  (let [lanternfish-freqs (into (sorted-map) (frequencies lanternfish))]
    (mapv #(get lanternfish-freqs % 0) (range 9))))

(defn next-lanterfish
  "Returns the new vector of the lanternfish after one day."
  [lanternfish]
  (let [days0-lanternfish (first lanternfish)
        new-days (subvec (conj lanternfish days0-lanternfish) 1)]
    (if (pos? (first lanternfish))
      (assoc new-days 6 (+' (get new-days 6) days0-lanternfish))
      new-days)))

(defn simulate
  "Runs the simulation for the given number of days-left."
  [days-left lanternfish]
  (if (zero? days-left)
    lanternfish
    (let [new-lanternfish (next-lanterfish lanternfish)]
      (recur (dec days-left) new-lanternfish))))

; --------------------------
; results

(defn day06-1
  []
  (apply +' (simulate 80 (init-lanternfish))))

(defn day06-2
  []
  (apply +' (simulate 256 (init-lanternfish))))

(defn -main
  []
  (println (day06-1))
  (println (day06-2)))
