(ns day20.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-image
  "Parses the collection of strings that represents the image (a string per row)
  and returns a map of [x y] -> value. Value is 1 for \\# and 0 for \\."
  [row-data rows columns]
  (let [locations (for [i (range rows)
                        j (range columns)]
                    [i j])]
    (reduce (fn [result loc]
              (if (= \# (get-in row-data loc))
                (assoc result loc 1)
                (assoc result loc 0)))
            {} locations)))

(defn parse-algorithm
  "Parses the string that represents the algorithm and returns a vector of 0 and 1.
  The \\# have been mapped to 1 and the \\. to 0."
  [s]
  (mapv #(if (= \# %) 1 0) s))

(defn parse
  "Parses the input string and returns a vector of 4 items. First item is the
  algorithm as returned by parse-algorithm. Second item is the image as returned by
  parse-image. Third and fourth items are the initial rows,columns of the image."
  [s]
  (let [split-lines (clojure.string/split-lines s)
        algorithm (first split-lines)
        image (vec (drop 2 split-lines))
        rows (count image)
        columns (count (first image))
        final-image (parse-image image rows columns)
        final-algorithm (parse-algorithm algorithm)]
    [final-algorithm final-image rows columns]))

(def parsed-input (parse (slurp input-file)))
(def algorithm (first parsed-input))
(def image (second parsed-input))
(def rows (get parsed-input 2))
(def columns (get parsed-input 3))

(defn -main
  []
  (println image))
