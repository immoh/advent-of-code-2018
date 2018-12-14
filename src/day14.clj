(ns day14
  (:require [clojure.string]))

(defn digits [n]
  (if (< n 10)
    [n]
    [1 (mod n 10)]))

(defn execute-round [{:keys [recipes elves]}]
  (let [current-recipes (mapv (partial get recipes) elves)
        new-recipes (into recipes (digits (reduce + current-recipes)))]
    {:recipes new-recipes
     :elves  (map #(mod (+ % %2 1) (count new-recipes)) elves current-recipes)}))

(defn min-recipes [recipes]
  (first (drop-while #(< (count (:recipes %)) recipes)
                     (iterate execute-round {:recipes [3 7] :elves [0 1]}))))

(defn part1 [recipes]
  (->> (min-recipes (+ recipes 10))
       (:recipes)
       (drop recipes)
       (take 10)
       (clojure.string/join)))
