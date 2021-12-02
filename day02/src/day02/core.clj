(ns day02.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n and converts it to the appropriate structure,
  a vector containing vectors, each one representing an instruction."
  [s]
  (->> s
       clojure.string/split-lines
       (mapv #(clojure.string/split % #" "))
       (mapv #(vector (keyword (first %)) (Integer/parseInt (second %))))))

(def instructions (parse (slurp input-file)))

; --------------------------
; problem 1

(defn follow-instructions
  "Starting in position 0,0, find the final position if the instructions are followed.
  (problem 1)"
  []
  (reduce (fn [[horizontal-pos depth] [command value]]
            (case command
              :forward [(+ horizontal-pos value) depth]
              :up [horizontal-pos (- depth value)]
              :down [horizontal-pos (+ depth value)]))
          [0 0] instructions))

; --------------------------
; problem 2

(defn follow-instructions2
  "Starting in position 0,0, find the final position if the instructions are followed
  (problem 2)"
  []
  (reduce (fn [[horizontal-pos depth aim] [command value]]
            (case command
              :forward [(+ horizontal-pos value) (+ depth (* aim value)) aim]
              :up [horizontal-pos depth (- aim value)]
              :down [horizontal-pos depth (+ aim value)]))
          [0 0 0] instructions))

; --------------------------
; results

(defn day02-1
  []
  (let [[horizontal-pos depth] (follow-instructions)]
    (* horizontal-pos depth)))

(defn day02-2
  []
  (let [[horizontal-pos depth _] (follow-instructions2)]
    (* horizontal-pos depth)))

(defn -main
  []
  (println (day02-1))
  (println (day02-2)))
