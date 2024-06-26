(ns day04.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-draw-numbers
  "Parses the string that represents the numbers drawn and returns a collection of numbers."
  [s]
  (let [draw-numbers (clojure.string/split s #",")]
    (map #(Integer/parseInt %) draw-numbers)))

(defn create-board
  "Creates a bingo board from a collection of 25 numbers (one row every 5 numbers).
  Returns a collection that contains 10 maps, one for each row or column.
  Map keys are the board numbers and their values are initialized to 0, indicating
  that all numbers are unmarked."
  [coll]
  (let [rows (map #(zipmap % (repeat 0)) (partition 5 coll))
        cols (->> coll
                  (zipmap (range 0 (count coll)))
                  (group-by #(mod (first %) 5))
                  vals
                  (map #(map second %))
                  (map #(zipmap % (repeat 0))))]
    (into rows cols)))

(defn parse-boards
  "Parses a collection of lines (strings) and returns an appropriate structure that represents
  all bingo boards. The lines are all the lines in the input file except the first one.
  Returns a collection of collections. Each collection is the return value of create-board."
  [coll]
  (->> coll
       (filter seq)
       (map #(clojure.string/split % #" "))
       (map #(filter seq %))
       flatten
       (map #(Integer/parseInt %))
       (partition 25)
       (map create-board)))

(defn parse
  "Splits the input string by newline and creates the structures that represent the drawn
  numbers and the bingo boards. Returns a vector of two items. First one is the draw-numbers
  as returned by parse-draw-numbers. Second one is the bingo boards as returned by parse-boards."
  [s]
  (let [split-s (clojure.string/split-lines s)
        draw-numbers (parse-draw-numbers (first split-s))
        boards (parse-boards (rest split-s))]
    [draw-numbers boards]))

(def starting-state (parse (slurp input-file)))
(def draw-numbers (first starting-state))
(def boards (set (second starting-state)))

(defn mark-row
  "Changes to 1 the mark of the number in the given row.
  Returns a map, keys are the numbers and their values
  is either 1 or 0 depending on whether the number is marked or not."
  [row number]
  (if-let [mark (get row number)]
    (if (zero? mark)
      (assoc row number 1)
      row)
    row))

(defn mark-board
  "Changes to 1 the mark of the number in every row/col of the board."
  [board number]
  (map #(mark-row % number) board))

(defn mark-all-boards
  "Changes to 1 the mark of the number in every board."
  [boards number]
  (set (map #(mark-board % number) boards)))

(defn all-marked?
  "Returns true if every number (key) in the row is marked (value = 1),
  false otherwise."
  [row]
  (= 5 (count (filter #{1} (vals row)))))

(defn winning?
  "Returns the board if the board is a winning board, nil otherwise."
  [board]
  (when (some true? (map all-marked? board))
    board))

(defn find-winnings
  "Finds all winning boards."
  [boards]
  (filter seq (map winning? boards)))

(defn collect-unmarked-row
  "Collects all unmarked numbers (value = 0) of the row."
  [row]
  (map first (filter #(zero? (second %)) row)))

(defn collect-unmarked-numbers
  "Collects all unmarked numbers (value = 0) of the board."
  [board]
  (reduce into #{} (map collect-unmarked-row board)))

(defn calculate-score
  "Calculates the score of the board."
  [[last-marked board]]
  (let [unmarked-numbers (collect-unmarked-numbers board)]
    (* last-marked (apply + unmarked-numbers))))

; --------------------------
; problem 1

(defn play-game1
  "Plays the game until there's a only one winning board. Returns a vector of two
  items. First item is the last marked number, second item is the winning board."
  []
  (loop [[number & rest-numbers] draw-numbers
         boards boards]
    (when number
      (let [new-boards (mark-all-boards boards number)
            winning-boards (find-winnings new-boards)]
        (if (seq winning-boards)
          (if (> (count winning-boards) 1)
            nil
            [number (first winning-boards)])
          (recur rest-numbers new-boards))))))

; --------------------------
; problem 2

(defn play-game2
  "Plays the game until there's only one board which wins last. Returns a vector
  of two items. First item is the last marked number, second item is the last
  winning board."
  []
  (loop [[number & rest-numbers] draw-numbers
         boards boards]
    (when number
      (let [new-boards (mark-all-boards boards number)
            winning-boards (find-winnings new-boards)]
        (if (seq winning-boards)
          (if (= 1 (count new-boards))
            [number (first winning-boards)]
            (recur rest-numbers (reduce disj new-boards winning-boards)))
          (recur rest-numbers new-boards))))))

; --------------------------
; results

(defn day04-1
  []
  (calculate-score (play-game1)))

(defn day04-2
  []
  (calculate-score (play-game2)))

(defn -main
  []
  (println (day04-1))
  (println (day04-2)))
