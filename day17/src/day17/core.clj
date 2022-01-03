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

(defn acceptable-velocities
  "Returns a vector of all velocities [x0 y0] that cause the probe to be within the target
  area after any step."
  []
  (let [xy-vals (for [x (range min-vel-x (inc max-vel-x))
                      y (range min-vel-y (inc max-vel-y))]
                  [x y])]
    (->> xy-vals
         (map #(and (inside-target? %) %))
         (filter (complement false?)))))

(defn find-max-height
  "Returns the max height the probe reaches given its initial y velocity."
  [vel-y0]
  (if (neg? vel-y0)
    0
    (/ (* vel-y0 (inc vel-y0)) 2)))

; --------------------------
; results

(defn day17-1-sol1
  []
  (->> (acceptable-velocities)
       (map #(find-max-height (second %)))
       (apply max)))

(defn day17-1-sol2
  []
  (/ (* target-ymin (inc target-ymin)) 2))

(defn -main
  []
  (println (time (day17-1-sol1)))
  (println (time (day17-1-sol2))))
