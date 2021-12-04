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

(defn most-common-bit
  "Returns the most common bit given the number of ones and zeros."
  [ones zeros]
  (if (>= ones zeros)
    \1
    \0))

(defn least-common-bit
  "Returns the least common bit given the number of ones and zeros."
  [ones zeros]
  (if (<= zeros ones)
    \0
    \1))

(defn find-common-bit
  "Finds the least or most common bit based on the second arg
  which is the corresponding least/most common bit function."
  [bits common-fn]
  (let [ones (count (filter #{\1} bits))
        zeros (- (count bits) ones)]
    (common-fn ones zeros)))

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

(defn calc-value
  "Calculates the required values for problem 1 (in binary).
  Accepts as an argument the least or most common bit function."
  [common-fn]
  (let [nth-digits (map #(get-nth-digit %) (range (count (first report))))]
    (map #(find-common-bit % common-fn) nth-digits)))

(defn calc-gamma
  "Calculates the gamma value."
  []
  (calc-value most-common-bit))

(defn calc-epsilon
  "Calculates the epsilon value."
  []
  (calc-value least-common-bit))

; --------------------------
; problem 2

(defn calc-value2
  "Calculates the required values for problem 2 (in binary).
  Accepts as an argument the least or most common bit function."
  [common-fn]
  (loop [i 0
         result report]
    (let [nth-digits (map #(get % i) result)
          common-digit (find-common-bit nth-digits common-fn)
          common (filter #(= common-digit (get % i)) result)]
      (if (= 1 (count common))
        (first common)
        (recur (inc i) common)))))

(defn calc-oxygen
  "Calculates the oxygen value."
  []
  (calc-value2 most-common-bit))

(defn calc-co2
  "Calculates the CO2 value."
  []
  (calc-value2 least-common-bit))

; --------------------------
; results

(defn day03-1
  []
  (let [gamma (bin->dec (calc-gamma))
        epsilon (bin->dec (calc-epsilon))]
    (* gamma epsilon)))

(defn day03-2
  []
  (let [oxygen (bin->dec (calc-oxygen))
        co2 (bin->dec (calc-co2))]
    (* oxygen co2)))

(defn -main
  []
  (println (day03-1))
  (println (day03-2)))
