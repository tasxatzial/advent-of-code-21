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

(def open-brackets #{\{ \( \[ \<})
(def close-brackets #{\} \) \] \>})

(def matching-bracket
  (zipmap open-brackets close-brackets))

(defn open-bracket?
  "Returns true if c is an opening bracket, false otherwise."
  [c]
  (contains? matching-bracket c))

(defn complete-chunk
  "Finds the missing chunk of an incomplete chunk."
  [unclosed-brackets]
  (reverse (map matching-bracket unclosed-brackets)))

(defn process-chunk
  "Returns true if the given string has properly matched brackets, false otherwise."
  [s]
  (loop [stack []
         [bracket & rest-brackets] s]
    (if bracket
      (if (open-bracket? bracket)
        (recur (conj stack bracket) rest-brackets)
        (let [prev-bracket (peek stack)]
          (if (= bracket (matching-bracket prev-bracket))
            (recur (pop stack) rest-brackets)
            [:corrupted bracket])))
      (if (empty? stack)
        [:valid []]
        [:incomplete (complete-chunk stack)]))))

(defn process-all-chunks
  []
  (map process-chunk chunks))

(def memoized-process-all-chunks (memoize process-all-chunks))

; --------------------------
; problem 1

(defn illegal-brackets
  []
  (filter #(= :corrupted (first %)) (memoized-process-all-chunks)))

(def illegal-bracket-vals
  {\) 3
   \] 57
   \} 1197
   \> 25137})

; --------------------------
; problem 2

(defn incomplete-chunks
  []
  (filter #(= :incomplete (first %)) (memoized-process-all-chunks)))

(def incomplete-bracket-vals
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn incomplete-chunk-val
  [chunk]
  (reduce (fn [result bracket]
            (+ (incomplete-bracket-vals bracket) (* 5 result)))
          0 chunk))

; --------------------------
; results

(defn day10-1
  []
  (->> (illegal-brackets)
       (map second)
       (map illegal-bracket-vals)
       (apply +)))

(defn day10-2
  []
  (let [vals (->> (incomplete-chunks)
                  (map second)
                  (map incomplete-chunk-val)
                  sort)]
    (nth vals (/ (dec (count vals)) 2))))

(defn -main
  []
  (println (day10-1))
  (println (day10-2)))
