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

; --------------------------
; problem 1

(defn next-caves1
  "Returns the caves that can be visited when we are in the last cave
  of the path (problem 1)."
  [cave path]
  (remove #(and (not (contains? large-caves %))
                (contains? (set path) %))
          (get cave-map cave)))

(defn expand-path1
  "Returns a vector of all paths that can be formed by appending
  each allowed next cave when we are in the last cave of the path (vector),
  to the end of the given path (problem 1).
  If there are 0 next caves and we are not in the :end cave, an empty
  vector is returned (path is incomplete)."
  [path]
  (if (= :end (last path))
    [path]
    (let [allowed-caves (next-caves1 (last path) path)]
      (if (seq allowed-caves)
        (reduce #(conj %1 (conj path %2)) [] allowed-caves)
        []))))

(defn -main
  []
  (println cave-map))
