(ns day03
  (:require [clojure.string]))

(defn parse-claim [row]
  (map #(Integer/parseInt %) (rest (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" row))))

(defn parse-input [input]
  (map parse-claim (clojure.string/split-lines input)))

(defn count-overlaps [usage-map]
  (count (filter (fn [[_ c]] (> c 1)) usage-map)))

(defn claim-squares [[_ start-x start-y dx dy]]
  (for [x (range start-x (+ start-x dx))
        y (range start-y (+ start-y dy))]
    [x y]))

(defn create-usage-map [claims]
  (frequencies (mapcat claim-squares claims)))

(defn part1 [input]
  (count-overlaps (create-usage-map (parse-input input))))

(defn has-no-overlap? [usage-map claim]
  (every? #{1} (map usage-map (claim-squares claim))))

(defn part2 [input]
  (let [claims (parse-input input)]
    (ffirst (filter (partial has-no-overlap? (create-usage-map claims)) claims))))
