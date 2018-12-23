(ns day22
  (:require [clojure.string]))

(declare erosion-level)

(defn geological-index* [depth target [x y :as p]]
  (cond
    (= p [0 0]) 0
    (= p target) 0
    (= y 0) (* x 16807)
    (= x 0) (* y 48271)
    :else (* (erosion-level depth target [x (dec y)])
             (erosion-level depth target [(dec x) y]))))

(def geological-index (memoize geological-index*))

(defn erosion-level* [depth target p]
  (mod (+ (geological-index depth target p) depth) 20183))

(def erosion-level (memoize erosion-level*))

(defn risk-level* [depth target p]
  (mod (erosion-level depth target p) 3))

(def risk-level (memoize risk-level*))

(defn part1 [depth [target-x target-y :as target]]
  (reduce + (for [x (range (inc target-x))
                  y (range (inc target-y))]
              (risk-level depth target [x y]))))

(defn neighbor-locations [location]
  (filter (partial every? nat-int?) (mapv (partial mapv + location) [[-1 0] [1 0] [0 -1] [0 1]])))

(defn valid-equipments [depth target location]
  (case (risk-level depth target location)
    0 #{:climbing-gear :torch}
    1 #{:climbing-gear :none}
    2 #{:torch :none}))

(defn neighbors [depth target {current-location :location}]
  (for [location (neighbor-locations current-location)
        equipment (valid-equipments depth target location)]
    {:location  location
     :equipment equipment}))

(defn calculate-distance [current current-distance neighbor]
  (+ current-distance 1 (if (= (:equipment current) (:equipment neighbor)) 0 7)))

(defn neighbor->distance [depth target current current-distance]
  (let [neighbors (neighbors depth target current)]
    (zipmap neighbors (map (partial calculate-distance current current-distance) neighbors))))

(defn update-distance [distances [neighbor distance]]
  (update distances neighbor (fn [current-distance]
                               (min (or current-distance distance) distance))))

(defn select-next [distances visited?]
  (->> distances
       (remove #(visited? (key %)))
       (sort-by val)
       (first)
       (key)))

(defn part2 [depth target]
  (loop [current {:location  [0 0]
                  :equipment :torch}
         distances {current 0}
         visited #{current}]
    (if (= current {:location  target
                    :equipment :torch})
      (get distances current)
      (let [neighbors (neighbor->distance depth target current (get distances current))
            new-distances (reduce (partial update-distance) distances neighbors)
            new-visited (conj visited current)]
        (recur (select-next new-distances new-visited)
               new-distances
               new-visited)))))
