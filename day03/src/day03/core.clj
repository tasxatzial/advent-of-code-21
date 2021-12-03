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

(defn find-most-common
  "Finds the most common bit."
  [bits]
  (let [ones (count (filter #{\1} bits))
        zeros (- (count bits) ones)]
    (if (>= ones zeros)
      \1
      \0)))

(defn find-least-common
  "Finds the least common bit."
  [bits]
  (let [ones (count (filter #{\1} bits))
        zeros (- (count bits) ones)]
    (if (<= zeros ones)
      \0
      \1)))

(defn -main
  []
  (println report))
