(ns day01.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n and converts it into a vector of numbers."
  [s]
  (->> s
       clojure.string/split-lines
       (mapv #(Integer/parseInt %))))

(def depths (parse (slurp input-file)))

(defn count-positive-diffs
  "Counts the number of times an item increases from the previous item."
  [coll]
  (let [diffs (map - coll (rest coll))]
    (count (filter neg? diffs))))

; --------------------------
; results

(defn day01-1
  []
  (count-positive-diffs depths))

(defn day01-2
  []
  (let [sums (map +  depths (rest depths) (rest (rest depths)))]
    (count-positive-diffs sums)))

(defn -main
  []
  (println (day01-1))
  (println (day01-2)))
