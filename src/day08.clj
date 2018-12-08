(ns day08
  (:require [clojure.string]))

(defn parse-input [input]
  (map #(Integer/parseInt %) (clojure.string/split input #" ")))

(defn ->tree [[child-count metadata-count & data]]
  (if (zero? child-count)
    (let [metadata (take metadata-count data)]
      {:metadata metadata
       :length   (+ metadata-count 2)
       :value    (reduce + metadata)})
    (loop [remaining-count child-count
           remaining-data data
           children []]
      (if (zero? remaining-count)
        (let [metadata (take metadata-count remaining-data)]
          {:metadata metadata
           :children children
           :length   (+ (reduce + (map :length children)) metadata-count 2)
           :value    (reduce + (map :value (keep #(get children (dec %)) metadata)))})
        (let [{:keys [length] :as child} (->tree remaining-data)]
          (recur (dec remaining-count)
                 (drop length remaining-data)
                 (conj children child)))))))

(defn part1 [input]
  (->> (parse-input input)
       (->tree)
       (tree-seq :children :children)
       (mapcat :metadata)
       (reduce +)))

(defn part2 [input]
  (->> (parse-input input)
       (->tree)
       :value))
