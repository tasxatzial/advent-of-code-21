(ns day04.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-draw-numbers
  "Parses the string that represents the numbers drawn and returns
  a vector of numbers."
  [s]
  (let [draw-numbers (clojure.string/split s #",")]
    (mapv #(Integer/parseInt %) draw-numbers)))

(defn -main
  []
  (println (parse-draw-numbers "23,91,18,32,73,14")))
