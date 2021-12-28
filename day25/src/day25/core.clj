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

(defn -main
  []
  (println sea-cucumbers))
