(ns day25.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Parses the input and returns a vector of 3 items. First item is the
  rows, second is the columns, third is a map of cucumber-location-> moving direction."
  [s]
  (let [split-lines (clojure.string/split-lines s)
        rows (count split-lines)
        columns (count (first split-lines))
        locations (for [i (range rows)
                        j (range columns)]
                    [i j])
        sea-cucumbers (flatten (map #(map identity %) split-lines))]
    [rows columns (zipmap locations sea-cucumbers)]))

(def parsed-input (parse (slurp input-file)))
(def rows (first parsed-input))
(def columns (second parsed-input))
(def sea-cucumbers (get parsed-input 2))

(defn collect-east-cucumbers
  "Returns a set of the locations of the cucumbers that move east."
  []
  (into #{} (map first (filter #(= \> (second %)) sea-cucumbers))))

(defn collect-south-cucumbers
  "Returns a set of the locations of the cucumbers that move south."
  []
  (into #{} (map first (filter #(= \v (second %)) sea-cucumbers))))

(defn next-east-loc
  "Returns the location to the east of the given location [x y]."
  [[x y]]
  (let [next-y (inc y)]
    (if (= next-y columns)
      [x 0]
      [x next-y])))

(defn next-south-loc
  "Returns the location to the south of the given location [x y]."
  [[x y]]
  (let [next-x (inc x)]
    (if (= next-x rows)
      [0 y]
      [next-x y])))

(defn move
  "Returns a set of the new locations of the given moving-cucumbers.
  fn_move: either next-east-loc or next-south-loc depending on whether
  the moving-cucumbers set is moving to the east or to the south."
  [moving-cucumbers other-cucumbers fn_move]
  (reduce (fn [result loc]
            (let [next-loc (fn_move loc)]
              (if (or (contains? other-cucumbers next-loc)
                      (contains? moving-cucumbers next-loc))
                (conj result loc)
                (conj result next-loc))))
          #{} moving-cucumbers))

(defn move-once
  "Returns a vector of two items. First item is a set of the new locations of the
  cucumbers moving east. Second item is a set of the new locations of the cucumbers
  moving south."
  [east-cucumbers south-cucumbers]
  (let [new-east-cucumbers (move east-cucumbers south-cucumbers next-east-loc)
        new-south-cucumbers (move south-cucumbers new-east-cucumbers next-south-loc)]
    [new-east-cucumbers new-south-cucumbers]))

(defn -main
  []
  (println sea-cucumbers))
