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

(defn find-paths
  "Accepts an init-path vector as the initial path collection and returns a
  vector of all distinct paths that can be formed.
  fn_expand-path: expands the current collection of paths.
  fn_next-caves: indicates what caves are allowed to be visited next."
  [init-path fn_expand-path]
  (loop [paths init-path]
    (let [new-paths (->> paths
                         (map #(fn_expand-path %))
                         (reduce into []))]
      (if (= paths new-paths)
        new-paths
        (recur new-paths)))))

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

; --------------------------
; problem 2

(defn twice-visited?
  "Returns true if there is a small cave in the path that has been visited twice,
  false otherwise."
  [path]
  (if (= 1 (first path))
    true
    (let [cave-freqs (frequencies path)]
      (boolean (some #(and (not (contains? large-caves (first %)))
                           (> (second %) 1))
                     cave-freqs)))))

(defn mark-path-if-twice-visited-cave
  "Changes the first item of the path from 0 to 1 if there is a small
  cave that appears twice."
  [path]
  (if (twice-visited? path)
    (assoc path 0 1)
    path))

(defn next-caves2
  "Returns the caves that can be visited when we are in the last cave
  of the path (problem 2)."
  [cave path]
  (let [twice-visited (twice-visited? path)
        next-caves-expect-start (remove #(= :start %) (get cave-map cave))]
    (if twice-visited
      (let [path-set (set path)]
        (remove #(and (not (contains? large-caves %))
                      (contains? path-set %))
                next-caves-expect-start))
      next-caves-expect-start)))

(defn expand-path2
  "Returns a vector of all paths that can be formed by appending
  each allowed next cave when we are in the last cave of the path (vector),
  to the end of the given path (problem 2).
  If there are 0 next caves and we are not in the :end cave, an empty
  vector is returned (path is incomplete)."
  [path]
  (if (= :end (last path))
    [path]
    (let [updated-path (mark-path-if-twice-visited-cave path)
          allowed-caves (next-caves2 (last updated-path) updated-path)]
      (if (seq allowed-caves)
        (reduce #(conj %1 (conj updated-path %2)) [] allowed-caves)
        []))))

; --------------------------
; results

(defn day12-1
  []
  (count (find-paths [[:start]] expand-path1)))

(defn day12-2
  []
  (count (find-paths [[0 :start]] expand-path2)))

(defn -main
  []
  (println (day12-1))
  (println (day12-2)))
