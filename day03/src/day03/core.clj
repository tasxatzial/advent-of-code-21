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

(defn get-nth-digit
  "Returns the nth char of each of the binary strings."
  [n]
  (map #(get % n) report))

(defn char->int
  "Converts a char to the corresponding integer."
  [c]
  (- (int c) 48))

(defn bin->dec
  "Converts a binary number (represented as a collection of \0 and \1)
  to decimal."
  [b]
  (let [pow2 (reverse (take (count b) (iterate (partial * 2) 1)))
        bin (map char->int b)]
    (apply + (map * pow2 bin))))

; --------------------------
; problem 1

(defn calc-gamma
  "Calculates the gamma value."
  []
  (let [nth-digits (map #(get-nth-digit %) (range (count (first report))))]
    (map find-most-common nth-digits)))

(defn calc-epsilon
  "Calculates the epsilon value."
  []
  (let [nth-digits (map #(get-nth-digit %) (range (count (first report))))]
    (map find-least-common nth-digits)))

; --------------------------
; results

(defn -main
  []
  (println (calc-gamma)))
