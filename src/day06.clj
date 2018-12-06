(ns day06
  (:require [clojure.set]
            [clojure.string]))

(defn parse-coordinates [line]
  (map #(Integer/parseInt %) (clojure.string/split line #", ")))

(defn parse-input [input]
  (map parse-coordinates (clojure.string/split-lines input)))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^int (- x2 x1)) (Math/abs ^int (- y2 y1))))

(defn find-closest-point [point points]
  (let [[closest1 closest2] (sort-by val (zipmap points
                                                  (map (partial manhattan-distance point) points)))]
    (when-not (= (val closest1) (val closest2))
      (key closest1))))

(defn create-area-map [points margin]
  (frequencies (keep identity (for [x (range (- (apply min (map first points)) margin)
                                             (+ (apply max (map first points)) margin 1))
                                    y (range (- (apply min (map second points)) margin)
                                             (+ (apply max (map second points)) margin 1))]
                                (find-closest-point [x y] points)))))
(defn find-fixed-areas [points]
  (clojure.set/intersection (set (create-area-map points 0)) (set (create-area-map points 1))))

(defn part1 [input]
  (let [points (parse-input input)]
    (-> (find-fixed-areas points)
        (vals)
        (sort)
        (last))))

(defn total-distance [points point]
  (reduce + (map (partial manhattan-distance point) points)))

(defn part2 [input]
  (let [points (parse-input input)]
    (->> (for [x (range (- (apply min (map first points)))
                        (+ (apply max (map first points)) 1))
               y (range (- (apply min (map second points)))
                        (+ (apply max (map second points)) 1))]
           [x y])
         (filter #(< (total-distance points %) 10000))
         (count))))
