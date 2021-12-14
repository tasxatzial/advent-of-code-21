(ns day14.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-polymer
  "Returns a seq from the given string polymer."
  [s]
  (seq s))

(defn parse-rules
  "Parses the input rules and returns a map of [\\A \\B] -> \\C.
  Each rule is a line in the input file."
  [s]
  (let [pairs (map #(clojure.string/split % #" -> ") s)]
    (reduce (fn [result [left-rule right-rule]]
              (conj result [(vec left-rule) (first right-rule)]))
            {} pairs)))

(defn parse
  "Parses the input string and returns a vector of two items. First item
  is the polymer as a seq. Second item is the insertion rules as returned
  by parse-rules."
  [s]
  (let [split-lines (clojure.string/split-lines s)
        polymer (first split-lines)
        rules (drop 2 split-lines)]
    [(parse-polymer polymer) (parse-rules rules)]))

(def parsed-input (parse (slurp input-file)))
(def polymer (first parsed-input))
(def rules (second parsed-input))

(defn -main
  []
  (println rules))
