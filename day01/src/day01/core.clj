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

; --------------------------
; results

(defn day01-1
  []
  (let [diffs (map - depths (rest depths))]
    (count (filter neg? diffs))))

(defn -main
  []
  (println (day01-1)))
