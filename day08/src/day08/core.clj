(ns day08.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-line
  "Parses an input line and returns a collection of two items. The first item is
  a vector of the signal patterns. The second item is a vector of the output values."
  [s]
  (let [split-line (clojure.string/split s #" [|] ")]
    (map #(clojure.string/split % #" ") split-line)))

(defn parse
  "Parses the input string into a collection that contains the results
  of parse-line when applied to each input line."
  [s]
  (let [split-input (clojure.string/split-lines s)]
    (map parse-line split-input)))

(def signals (parse (slurp input-file)))

(def segments->digit
  {#{1 2 3 5 6 7} 0
   #{3 6} 1
   #{1 3 4 5 7} 2
   #{1 3 4 6 7} 3
   #{2 3 4 6} 4
   #{1 2 4 6 7} 5
   #{1 2 4 5 6 7} 6
   #{1 3 6} 7
   #{1 2 3 4 5 6 7} 8
   #{1 2 3 4 6 7} 9})

; --------------------------
; problem 1

(defn unique-segment-count
  "Returns the segment counts that appear one time."
  []
  (->> segments->digit
       keys
       (map count)
       frequencies
       (filter #(= 1 (second %)))
       (map first)
       set))


; --------------------------
; results

(defn day08-1
  []
  (let [unique-segment-count (unique-segment-count)]
    (->> (map second signals)
         (map #(map (fn [x] (count x)) %))
         (map #(filter unique-segment-count %))
         (map count)
         (apply +))))

(defn -main
  []
  (println (day08-1)))
