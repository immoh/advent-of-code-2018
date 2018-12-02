(ns day01
  (:require [clojure.string]))

(defn parse-input [input]
  (map #(Integer/parseInt %) (clojure.string/split-lines input)))

(defn find-first-duplicate [coll]
  (loop [seen #{}
         coll coll]
    (let [x (first coll)]
      (if (seen x)
        x
        (recur (conj seen x)
               (rest coll))))))

(defn part1 [input]
  (reduce + (parse-input input)))

(defn part2 [input]
  (find-first-duplicate (reductions + 0 (cycle (parse-input input)))))
