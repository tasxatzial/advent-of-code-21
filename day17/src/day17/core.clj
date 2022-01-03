(ns day17.core
  (:gen-class))

; the solution below depends on the input

; --------------------------
; common

(def target-xmin 288)
(def target-xmax 330)
(def target-ymax -50)
(def target-ymin -96)
(def min-vel-x (Math/round (Math/floor (/ (dec (Math/sqrt (inc (* 8 target-xmin)))) 2))))
(def max-vel-x target-xmax)
(def min-vel-y target-ymin)
(def max-vel-y (- target-ymin))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
