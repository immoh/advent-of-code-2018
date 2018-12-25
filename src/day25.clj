(ns day25
  (:require [clojure.set]
            [clojure.string]))

(defn parse-line [line]
  (mapv #(Integer/parseInt %) (clojure.string/split line #",")))

(defn parse-input [input]
  (map parse-line (clojure.string/split-lines input)))

(defn manhattan-distance [p1 p2]
  (reduce + (map #(Math/abs ^int (- % %2)) p1 p2)))

(defn in-constellation? [constellation point]
  (<= (reduce min (map (partial manhattan-distance point) constellation)) 3))

(defn add-point-to-constellations [constellations point]
  (let [{in-same-constellation true other-constellations false} (group-by #(in-constellation? % point) constellations)]
    (conj other-constellations (reduce clojure.set/union #{point} in-same-constellation))))

(defn constellations [points]
  (reduce
    add-point-to-constellations
    [#{(first points)}]
    (rest points)))

(defn part1 [input]
  (count (constellations (parse-input input))))
