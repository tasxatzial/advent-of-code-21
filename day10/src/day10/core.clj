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

(defn -main
  []
  (println chunks))
