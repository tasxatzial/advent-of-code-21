(ns day02.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n and converts it to the appropriate structure,
  a vector containing vectors, each one representing an instruction."
  [s]
  (->> s
       clojure.string/split-lines
       (mapv #(clojure.string/split % #" "))
       (mapv #(vector (keyword (first %)) (Integer/parseInt (second %))))))

(def instructions (parse (slurp input-file)))

(defn -main
  []
  (println instructions))
