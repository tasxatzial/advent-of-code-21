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

(defn initialize-pair-frequencies
  "Initializes a map with the frequencies of the polymer pairs."
  []
  (frequencies (map #(vector %1 %2) polymer (rest polymer))))

(defn next-pair-frequencies
  "Returns the new frequencies of the polymer pairs after one step."
  [pair-frequencies]
  (reduce (fn [result [[left-letter right-letter :as pair] frequency]]
            (let [new-letter (get rules pair)
                  new-left [left-letter new-letter]
                  new-right [new-letter right-letter]
                  new-left-frequency (get result new-left 0)
                  new-right-frequency (get result new-right 0)]
              (-> result
                  (assoc new-left (+ frequency new-left-frequency))
                  (assoc new-right (+ frequency new-right-frequency)))))
          {} pair-frequencies))

(defn simulate
  "Runs the simulation for the given number of steps.
  Returns a final map of the polymer pair frequencies."
  ([steps]
   (simulate steps (initialize-pair-frequencies)))
  ([steps pair-frequencies]
   (if (pos? steps)
     (recur (dec steps) (next-pair-frequencies pair-frequencies))
     pair-frequencies)))

(defn collect-letter-frequencies
  "Takes a map of the frequencies of the polymer pairs and returns a
  map of the frequencies of each letter. The frequency of each letter
  is the sum of the frequencies of the pairs that contain the letter."
  [pair-frequencies]
  (let [letters (into #{} (flatten (map first pair-frequencies)))
        letter-frequencies (zipmap letters (repeat 0))]
    (reduce (fn [result [[left right] frequency]]
              (-> result
                  (update left #(+ % frequency))
                  (update right #(+ % frequency))))
            letter-frequencies pair-frequencies)))

(defn finalize-letter-frequencies
  "Receives a map of initial letter frequencies as returned by
  collect-letter-frequencies and calculates the final letter frequencies.
  The final frequency of a letter is calculated as:
  1) If it is not the first or last letter of the initial polymer:
     divide the initial frequency by 2.
  2) If it is the first or the last letter (but not both) of the initial polymer:
     add 1 to the initial frequency and divide by 2.
  3) If it is both the first and last letter of the initial polymer:
     add 2 to the initial frequency and divide by 2."
  [letter-frequencies]
  (let [zero-frequencies (zipmap (keys letter-frequencies) (repeat 0))
        init-frequencies (-> zero-frequencies
                             (update (first polymer) inc)
                             (update (last polymer) inc))]
    (reduce (fn [result [letter frequency]]
              (update result letter #(/ (+ % frequency) 2)))
            init-frequencies letter-frequencies)))

(defn -main
  []
  (println (simulate 10)))
