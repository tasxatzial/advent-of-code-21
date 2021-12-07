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

(defn -main
  []
  (println positions))
