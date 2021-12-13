(ns day12.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Parses the input string and returns a collection of vectors. Each vector
  has two keywords and corresponds to a line in the input file."
  [s]
  (->> s
       clojure.string/split-lines
       (map #(clojure.string/split % #"-"))
       (map #(vector (keyword (first %)) (keyword (second %))))))

(defn create-cave-map
  "Reads the parsed input and creates a map that describes the cave map.
  Key: cave keyword.
  Value: collection of all caves connected to the cave denoted by the key."
  [parsed-input]
  (let [cave-names (reduce into parsed-input)
        init-map (zipmap cave-names (repeat []))]
    (reduce (fn [result [cave1 cave2]]
              (-> result
                  (update cave1 #(conj % cave2))
                  (update cave2 #(conj % cave1))))
            init-map parsed-input)))

(def cave-map
  (-> input-file
      slurp
      parse
      create-cave-map
      (assoc :end [])))

; a set that contains all the keywords that correspond to the large caves
(def large-caves
  (->> (keys cave-map)
       (filter #(= (name %) (clojure.string/upper-case (name %))))
       set))

(defn -main
  []
  (println cave-map))
