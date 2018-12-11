(ns day11
  (:require [clojure.string]))

(defn calculate-total-power [power-level-map [x y] d]
  (reduce + (for [x' (range x (+ x d))
                  y' (range y (+ y d))]
              (get power-level-map [x' y']))))

(defn create-total-power-map [power-level-map]
  (reduce
    (fn [m p]
      (assoc m p (calculate-total-power power-level-map p 3)))
    {}
    (for [x (range 1 299)
          y (range 1 299)]
      [x y])))

(defn hundreds-digit [n]
  (mod (int (/ n 100)) 10))

(defn calculate-power-level [x y grid-serial-number]
  (let [rack-id (+ x 10)]
    (-> (* rack-id y)
        (+ grid-serial-number)
        (* rack-id)
        (hundreds-digit)
        (- 5))))

(defn create-power-level-map [grid-serial-number]
  (reduce
    (fn [m [x y]]
      (assoc m [x y] (calculate-power-level x y grid-serial-number)))
    {}
    (for [x (range 1 301)
          y (range 1 301)]
      [x y])))

(defn part1 [grid-serial-number]
  (->> (create-power-level-map grid-serial-number)
       (create-total-power-map)
       (sort-by val)
       (last)
       (key)
       (clojure.string/join ",")))
