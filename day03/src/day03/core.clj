(ns day03.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by lines."
  [s]
  (clojure.string/split-lines s))

(def report (parse (slurp input-file)))

(defn -main
  []
  (println report))
