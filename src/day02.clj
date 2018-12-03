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

(defn find-differing-indices [id1 id2]
  (keep-indexed (fn [i c1]
                  (when-not (= c1 (get id2 i))
                    i))
                id1))

(defn common-letters [id differing-index]
  (apply str (keep-indexed (fn [i c]
                             (when-not (= i differing-index)
                               c))
                           id)))

(defn part2 [input]
  (let [ids (parse-input input)]
    (loop [id-pairs (for [id1 ids id2 ids] [id1 id2])]
      (let [[id1 id2] (first id-pairs)
            differing-indices (find-differing-indices id1 id2)]
        (if (= (count differing-indices) 1)
          (common-letters id1 (first differing-indices))
          (recur (rest id-pairs)))))))
