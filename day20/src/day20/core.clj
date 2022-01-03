(ns day20.core
  (:gen-class))

; solution depends on the input

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

; powers of two: (256 128 ... 1)
(def powers2 (reverse (take 9 (iterate (partial * 2) 1))))

(defn bin->dec
  "Converts a binary number to decimal."
  [binary]
  (apply + (map * powers2 binary)))

(defn get-square
  "Returns a vector that contains the 9 locations that form a 3x3 square around [x y]."
  [[x y]]
  (let [top-left [(dec x) (dec y)]
        top [(dec x) y]
        top-right [(dec x) (inc y)]
        left [x (dec y)]
        center [x y]
        right [x (inc y)]
        bottom-left [(inc x) (dec y)]
        bottom [(inc x) y]
        bottom-right [(inc x) (inc y)]]
    [top-left top top-right left center right bottom-left bottom bottom-right]))

; --------------------------
; solution 1

(declare memoized-pixel-value)

(defn pixel-value
  "Returns the value of the pixel (0 or 1) at the given location after the image has been
  enhanced step times."
  [loc step]
  (if (zero? step)
    (get image loc 0)
    (let [loc-square (get-square loc)
          new-square-values (map #(memoized-pixel-value % (dec step)) loc-square)
          decimal (bin->dec new-square-values)]
      (get algorithm decimal))))

(def memoized-pixel-value (memoize pixel-value))

(defn image-range
  "Returns all the locations that are part of the image after enhancing it step times."
  [steps]
  (for [i (range (- steps) (+ columns steps))
        j (range (- steps) (+ rows steps))]
    [i j]))

; --------------------------
; solution 2

(def borders {:top-row 0
              :bottom-row (dec rows)
              :left-column 0
              :right-column (dec columns)})

(defn expand-borders
  "Expands each image border by 1. Left and top borders are decreased by 1.
  Bottom and right borders are increased by 1."
  [borders]
  {:top-row (dec (:top-row borders))
   :bottom-row (inc (:bottom-row borders))
   :left-column (dec (:left-column borders))
   :right-column (inc (:right-column borders))})

(defn expand-image
  "Expands the image by adding to it the pixels around the current image border.
  The value of those new pixels is the background-bit (0 or 1)."
  [image {:keys [top-row bottom-row left-column right-column]} background-bit]
  (let [top-bottom-range (range (dec left-column) (+ 2 right-column))
        left-right-range (range top-row (inc bottom-row))
        top-border (for [j top-bottom-range]
                     [(dec top-row) j])
        bottom-border (for [j top-bottom-range]
                        [(inc bottom-row) j])
        left-border (for [i left-right-range]
                      [i (dec left-column)])
        right-border (for [i left-right-range]
                       [i (inc right-column)])]
    (-> image
        (into (zipmap top-border (repeat background-bit)))
        (into (zipmap bottom-border (repeat background-bit)))
        (into (zipmap left-border (repeat background-bit)))
        (into (zipmap right-border (repeat background-bit))))))

(defn enhance-pixel
  "Returns the new value of a pixel at loc after enhancing it once."
  [image loc background-bit]
  (let [loc-square (get-square loc)
        square-values (map #(get image % background-bit) loc-square)
        decimal (bin->dec square-values)]
    (get algorithm decimal)))

(defn enhance-image
  "Enhances the image steps times."
  ([steps]
   (enhance-image steps image 0 borders))
  ([image background-bit]
   (reduce (fn [result pixel]
             (let [loc (first pixel)
                   new-pixel-value (enhance-pixel image loc background-bit)]
               (assoc result loc new-pixel-value)))
           {} image))
  ([steps image background-bit borders]
   (if (zero? steps)
     image
     (let [new-borders (expand-borders borders)
           expanded-image (expand-image image borders background-bit)
           enhanced-image (enhance-image expanded-image background-bit)
           new-background-bit (if (= 1 background-bit) 0 (get algorithm 0))]
       (recur (dec steps) enhanced-image new-background-bit new-borders)))))

; --------------------------
; results

(defn day20_sol1
  [steps]
  (let [img-range (image-range steps)]
    (apply + (map #(pixel-value % steps) img-range))))

(defn day20_sol2
  [steps]
  (let [enhanced-image (enhance-image steps)
        pixel-values (map second enhanced-image)]
    (apply + pixel-values)))

(defn -main
  []
  (println (time (day20_sol1 2)))
  (println (time (day20_sol2 2)))
  (println (time (day20_sol1 50)))
  (println (time (day20_sol2 50))))
