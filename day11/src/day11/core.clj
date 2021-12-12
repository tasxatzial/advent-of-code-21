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

(defn find-flashes
  "Collects all octopuses that will flash in the current step."
  [octopuses]
  (filter #(and (not (second (second %)))
                (> (first (second %)) 9))
          octopuses))

(defn flash-and-reset
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

(defn increase-adjacent
  "Increases the energies of all octopuses contained in the adjacent-octopuses map by their value."
  [octopuses adjacent-octopuses]
  (reduce (fn [result [pos freq]]
            (let [[energy flashed] (get octopuses pos)]
              (if (not flashed)
                (assoc result pos [(+ energy freq) false])
                result)))
          octopuses adjacent-octopuses))

(defn next-step
  "Advances the simulation by one step."
  [octopuses]
  (let [flash-octopuses (find-flashes octopuses)]
    (if (seq flash-octopuses)
      (let [updated-flashed (flash-and-reset flash-octopuses octopuses)
            adjacent (collect-adjacent-to-flash flash-octopuses)
            updated-adjacent (increase-adjacent updated-flashed adjacent)]
        (recur updated-adjacent))
      octopuses)))

(defn increase-energies
  "Increases all octopuses energies by 1."
  [octopuses]
  (reduce (fn [result [pos [energy flashed]]]
            (conj result [pos [(inc energy) flashed]]))
          {} octopuses))

(defn reset-flashed
  "Marks all octopuses as not flashed at the end of each step."
  [octopuses]
  (reduce (fn [result [pos [energy _]]]
            (assoc result pos [energy false]))
          {} octopuses))

; --------------------------
; problem 1

(defn count-flashes
  "Counts the number of flashes in the current step."
  [octopuses]
  (count (filter #(= true (second (second %))) octopuses)))

(defn simulate
  "Runs the simulation for the specified number of steps (problem 1).
  Returns a vector of the final octopuses and the total flashes that occurred during
  the simulation."
  ([energies steps]
   (simulate energies steps 0))
  ([energies steps flashes]
   (if (pos? steps)
     (let [increased-energies (increase-energies energies)
           energies-after-flashes (next-step increased-energies)
           flash-count (+ flashes (count-flashes energies-after-flashes))
           reset-flashed (reset-flashed energies-after-flashes)]
       (recur reset-flashed (dec steps) flash-count))
     [energies flashes])))

; --------------------------
; problem 2

(defn simulate2
  "Returns the step during which all octopuses flash."
  ([energies]
   (simulate2 energies 1))
  ([energies steps]
   (let [increased-energies (increase-energies energies)
         energies-after-flashes (next-step increased-energies)
         flash-count (count-flashes energies-after-flashes)]
     (if (= flash-count octopuses-count)
       steps
       (let [reset-flashed (reset-flashed energies-after-flashes)]
         (recur reset-flashed (inc steps)))))))

; --------------------------
; results

(defn day11-1
  []
  (second (simulate octopuses 100)))

(defn day11-2
  []
  (simulate2 octopuses))

(defn -main
  []
  (println (day11-1))
  (println (day11-2)))
