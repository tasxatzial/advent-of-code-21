(ns day06.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Parses the input file and returns collection of numbers."
  [s]
  (let [split-lines (clojure.string/split-lines s)
        ages (first split-lines)]
    (mapv #(Integer/parseInt %) (clojure.string/split ages #","))))

(def ages (parse (slurp input-file)))

(defn -main
  []
  (println ages))
