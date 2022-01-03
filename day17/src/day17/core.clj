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

(defn inside-target?
  "Returns true if the given initial velocity causes the probe to be within the target
  area after any step."
  [[vel-x0 vel-y0]]
  (loop [vel-y vel-y0
         vel-x vel-x0
         x 0
         y 0]
    (let [y-prev (- x (inc vel-y))
          x-new (if (pos? vel-x) (dec vel-x) 0)]
      (cond
        (and (> y-prev x) (< x target-ymin)) false
        (and (<= target-ymin x target-ymax)
             (<= target-xmin y target-xmax)) true
        :else (recur (dec vel-y) x-new (+ x vel-y) (+ y vel-x))))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
