(ns day10.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Parses the input string and returns a collection of strings (chunks)."
  [s]
  (let [split-lines (clojure.string/split-lines s)]
    split-lines))

(def chunks (parse (slurp input-file)))

(def open-chars #{\{ \( \[ \<})
(def close-chars #{\} \) \] \>})

(def matching-char
  (zipmap open-chars close-chars))

(defn open-bracket?
  "Returns true if c is an opening char, false otherwise."
  [c]
  (contains? matching-char c))

(defn complete-chunk
  "Finds the missing chunk of an incomplete chunk."
  [unclosed-chars]
  (reverse (map matching-char unclosed-chars)))

(defn process-chunk
  "Parses a chunk and returns:
  1) [:corrupted illegal-char] if the chunk is corrupted.
  2) [:valid []] if the chunk is valid.
  3) [:incomplete unclosed-chars] if the chunk is incomplete."
  [s]
  (loop [stack []
         [char & rest-chars] s]
    (if char
      (if (open-bracket? char)
        (recur (conj stack char) rest-chars)
        (let [prev-char (peek stack)]
          (if (= char (matching-char prev-char))
            (recur (pop stack) rest-chars)
            [:corrupted char])))
      (if (empty? stack)
        [:valid []]
        [:incomplete stack]))))

(defn process-all-chunks
  "Applies process-chunk to the chunks collection."
  []
  (map process-chunk chunks))

(def memoized-process-all-chunks (memoize process-all-chunks))

; --------------------------
; problem 1

(defn illegal-chars
  "Returns a collection of the illegal chars, one for each corrupted chunk."
  []
  (->> (memoized-process-all-chunks)
       (filter #(= :corrupted (first %)))
       (map second)))

(def illegal-char-val
  {\) 3
   \] 57
   \} 1197
   \> 25137})

; --------------------------
; problem 2

(defn incomplete-chunks
  "Returns a collection the unclosed char sequences, one sequence for each
  incomplete chunk."
  []
  (->> (memoized-process-all-chunks)
       (filter #(= :incomplete (first %)))
       (map second)))

(def incomplete-char-val
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn incomplete-chunk-val
  "Returns the score of an incomplete chunk."
  [unclosed-chars]
  (let [missing-chars (complete-chunk unclosed-chars)]
    (reduce (fn [result char]
              (+ (incomplete-char-val char) (* 5 result)))
            0 missing-chars)))

; --------------------------
; results

(defn day10-1
  []
  (->> (illegal-chars)
       (map illegal-char-val)
       (apply +)))

(defn day10-2
  []
  (let [vals (->> (incomplete-chunks)
                  (map incomplete-chunk-val)
                  sort)]
    (nth vals (/ (dec (count vals)) 2))))

(defn -main
  []
  (println (day10-1))
  (println (day10-2)))
