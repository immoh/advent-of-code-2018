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
