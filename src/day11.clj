(ns day11
  (:require [clojure.string]))

(defn max-by [f coll]
  (reduce
    (fn [{:keys [max-value] :as current-max} item]
      (let [value (f item)]
        (if (> value max-value)
          {:max-value value
           :max-item  item}
          current-max)))
    {:max-value (f (first coll))
     :max-item  (first coll)}
    (rest coll)))

(defn total3x3 [power-level-sum-map [x y]]
  (reduce
    +
    (map
      (fn [ys]
        (- (nth ys (+ y 2)) (nth ys (dec y) 0)))
      (take 3 (drop x power-level-sum-map)))))

(defn find-maximum-3x3-square [power-level-sum-map]
  (max-by
    (partial total3x3 power-level-sum-map)
    (for [x (range 0 298)
          y (range 0 298)]
      [x y])))

(defn create-power-level-sum-map [power-level-map]
  (map (partial reductions +) power-level-map))

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
  (mapv (fn [x]
          (mapv (fn [y]
                  (calculate-power-level x y grid-serial-number))
                (range 1 301)))
        (range 1 301)))

(defn part1 [grid-serial-number]
  (->> (create-power-level-map grid-serial-number)
       (create-power-level-sum-map)
       (find-maximum-3x3-square)
       (map inc)
       (clojure.string/join ",")))

(defn totalnxn [power-level-sum-map [x y n]]
  (reduce
    +
    (map
      (fn [ys]
        (- (nth ys (+ y n -1)) (nth ys (dec y) 0)))
      (take n (drop x power-level-sum-map)))))

(defn find-maximum-nxn-square-at-position [power-level-sum-map [x y]]
  (max-by
    #(totalnxn power-level-sum-map [x y %])
    (range 1 (- 301 (max x y)))))

(defn find-maximum-nxn-square [power-level-sum-map]
  (->> (pmap (partial find-maximum-nxn-square-at-position power-level-sum-map)
             (for [x (range 0 298)
                   y (range 0 298)]
               [x y]))
       (sort-by :max-value)
       (last)
       (:max-item)))

(defn fix-coordinates [[x y n]]
  [(inc x) (inc y) n])

(defn part2 [grid-serial-number]
  (->> (create-power-level-map grid-serial-number)
       (create-power-level-sum-map)
       (find-maximum-nxn-square)
       (fix-coordinates)
       (clojure.string/join ",")))
