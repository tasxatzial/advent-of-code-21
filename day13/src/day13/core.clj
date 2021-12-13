(ns day13.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-points
  "Points is a collection of strings, each string represents a coordinate and
  corresponds to a line in the input file.
  Returns a set of (x y) coordinates."
  [points]
  (->> points
       (map #(clojure.string/split % #","))
       flatten
       (map #(Integer/parseInt %))
       (partition 2)
       set))

(defn parse-folds
  "Folds is a collection of strings, each string represents a folding instruction and
  corresponds to a line in the input file.
  Returns a collection of [x|y fold-axis] instructions."
  [folds]
  (let [split-folds (->> folds
                         (map #(clojure.string/split % #" "))
                         (map #(drop 2 %))
                         flatten
                         (map #(clojure.string/split % #"=")))]
    (reduce (fn [res [c num]]
              (conj res [(first c) (Integer/parseInt num)]))
            [] split-folds)))

(defn parse
  "Parses the input string and returns a vector of two items.
  First item is a collection of the coordinates of the paper dots as returned by
  parse-points. Second item is a collection of the folding instructions as returned by
  parse-folds."
  [s]
  (let [split-lines (clojure.string/split-lines s)
        points (take-while #(not= "" %) split-lines)
        folds (rest (drop-while #(not= "" %) split-lines))]
    [(parse-points points) (parse-folds folds)]))

(def parsed-data (parse (slurp input-file)))
(def dots (set (first parsed-data)))
(def fold-instructions (second parsed-data))

(defn fold-y-axis
  "Folds the paper along the given y axis (the bottom part is folded up).
  Returns a set of the new coordinates of the dots."
  [dots axis]
  (reduce (fn [result [x y]]
            (if (> y axis)
              (let [new-y (+ axis (- axis y))]
                (conj result [x new-y]))
              (conj result [x y])))
          #{} dots))

(defn fold-x-axis
  "Folds the paper along the given x axis (the right part is folded left).
  Returns a set of the new coordinates of the dots."
  [dots axis]
  (reduce (fn [result [x y]]
            (if (> x axis)
              (let [new-x (+ axis (- axis x))]
                (conj result [new-x y]))
              (conj result [x y])))
          #{} dots))

(defn fold
  "Folds the paper along the c axis (\\x or \\y).
  Returns a set of the new coordinates of the dots."
  [dots [c axis]]
  (case c
    \x (fold-x-axis dots axis)
    \y (fold-y-axis dots axis)))

(defn -main
  []
  (println fold-instructions))
