(ns day21.core
  (:gen-class))

; --------------------------
; common

(def player1-pos 8)
(def player2-pos 10)

(defn dice-sum1
  "Returns the sum of the 3 roll results (problem 1)."
  [turn]
  (+ 6 (* 9 (dec turn))))

(defn next-pos
  "Returns the next position.
  pos: current player position
  sum: sum of the 3 roll results."
  [pos sum]
  (let [new-pos (rem (+ pos sum) 10)]
    (if (zero? new-pos)
      10
      new-pos)))

(defn move
  "Returns a vector of two items. First item is the new score of the player,
  second item is the new position of the player.
  score: current player score
  pos: current player position"
  [turn score pos]
  (let [dice-sum (dice-sum1 turn)
        new-pos (next-pos pos dice-sum)
        new-score (+ score new-pos)]
    [new-score new-pos]))

(defn play1
  "Play the game until there is a winner. Returns the product of the score of the
  losing player and the number of times the die was rolled (problem 1)."
  []
  (loop [turn 1
         players {:p1 [0 player1-pos]
                  :p2 [0 player2-pos]}]
    (let [curr-player (if (odd? turn) :p1 :p2)
          [curr-score curr-pos] (curr-player players)
          [new-score _ :as updated-player] (move turn curr-score curr-pos)
          new-players (assoc players curr-player updated-player)]
      (if (>= new-score 1000)
        (if (= :p1 curr-player)
          (* 3 turn (first (:p2 new-players)))
          (* 3 turn (first (:p1 new-players))))
        (recur (inc turn) new-players)))))

; --------------------------
; results

(defn day20-1
  []
  (play1))

(defn -main
  []
  (println (day20-1)))
