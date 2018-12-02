(ns day02
  (:require [clojure.string]))

(defn parse-input [input]
  (clojure.string/split-lines input))

(defn id->letter-freqs [id]
  (set (vals (frequencies id))))

(defn part1 [input]
  (let [letter-freqs (map id->letter-freqs (parse-input input))]
    (* (count (filter #(contains? % 2) letter-freqs))
       (count (filter #(contains? % 3) letter-freqs)))))
