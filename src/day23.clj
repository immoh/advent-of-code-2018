(ns day23
  (:require [clojure.string]))

(defn parse-line [line]
  (let [[_ x y z r] (re-find #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" line)]
    {:position [(Integer/parseInt x) (Integer/parseInt y) (Integer/parseInt z)]
     :radius (Integer/parseInt r)}))

(defn parse-input [input]
  (map parse-line (clojure.string/split-lines input)))

(defn distance [nanobot1 nanobot2]
  (reduce + (map #(Math/abs ^int (- % %2)) (:position nanobot1) (:position nanobot2))))

(defn in-range? [nanobot1 nanobot2]
  (<= (distance nanobot1 nanobot2) (:radius nanobot1)))

(defn part1 [input]
  (let [nanobots (parse-input input)
        strongest (last (sort-by :radius nanobots))]
    (count (filter (partial in-range? strongest) nanobots))))
