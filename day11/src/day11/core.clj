(ns day11.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn char->int
  "Converts a char digit to the corresponding integer."
  [c]
  (- (int c) 48))

(defn parse
  "Parses the input string and returns a map, keys are the octopus positions
  (0 to row * column) and values are vectors describing their initial states
  [initial-energy flashed]. Flashed is true only when an octopus flashed in the
  current step."
  [s]
  (let [split-lines (clojure.string/split-lines s)
        rows (count split-lines)
        columns (count (first split-lines))
        octopus-states (map #(vector (char->int %) false) (apply str split-lines))
        octopus-positions (range (* rows columns))]
    [rows columns (zipmap octopus-positions octopus-states)]))

(def parsed-input (parse (slurp input-file)))
(def octopuses (get parsed-input 2))
(def rows (get parsed-input 0))
(def columns (get parsed-input 1))
(def octopuses-count (* rows columns))

(defn pos->index
  "Maps position [x y] to the appropriate index in a one dimensional vector."
  [[i j]]
  (+ j (* i columns)))

(defn get-adjacent
  "Returns the 8 adjacent positions to [x y]."
  [pos]
  (let [i (quot pos columns)
        j (mod pos columns)
        adj-pos (for [k (range (dec i) (+ i 2))
                      p (range (dec j) (+ j 2))]
                  [k p])
        valid-adj-pos (filter #(and (<= 0 (first %) (dec rows))
                                    (<= 0 (second %) (dec columns)))
                              adj-pos)]
    (map pos->index valid-adj-pos)))

(def memoized-get-adjacent (memoize get-adjacent))

(defn find-flash-octopuses
  "Collects all octopuses that will flash in the current step."
  [octopuses]
  (filter #(and (not (second (second %)))
                (> (first (second %)) 9))
          octopuses))

(defn flash-and-reset-octopuses
  "Sets the state of each octopus in flash-octopuses to [0 true]. 0 is the new state,
  true indicates that it flashed."
  [flash-octopuses octopus]
  (reduce (fn [result [pos _]]
            (assoc result pos [0 true]))
          octopus flash-octopuses))

(defn collect-adjacent-to-flash
  "Returns a map of all octopuses that are adjacent to the octopuses in flash-octopuses.
  The value of each octopus denotes how many times it is adjacent."
  [flash-octopuses]
  (frequencies (reduce (fn [result [pos _]]
                         (into result (memoized-get-adjacent pos)))
                       [] flash-octopuses)))

(defn increase-adjacent-octopuses
  "Increases the energies of all octopuses contained in the adjacent-octopuses map by their value."
  [octopuses adjacent-octopuses]
  (reduce (fn [result [pos freq]]
            (let [[energy flashed] (get octopuses pos)]
              (if (not flashed)
                (assoc result pos [(+ energy freq) false])
                result)))
          octopuses adjacent-octopuses))

(defn -main
  []
  (println octopuses))
