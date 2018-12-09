(ns day09
  (:require [clojure.string]))

(defn parse-input [input]
  (map #(Integer/parseInt %) (re-seq #"\d+" input)))

(defn play-turn [{:keys [scores circle current]} [player marble]]
  (if (zero? (mod marble 23))
    (let [index-to-remove (mod (- current 7) (count circle))
          marble-to-remove (get circle index-to-remove)]
      {:circle  (vec (concat (take index-to-remove circle) (drop (inc index-to-remove) circle)))
       :current index-to-remove
       :scores  (update scores player (fnil + 0) marble marble-to-remove)})
    (let [new-index (inc (mod (inc current) (count circle)))]
      {:circle  (vec (concat (take new-index circle) [marble] (drop new-index circle)))
       :current new-index
       :scores  scores})))

(defn play-game [players last-marble]
  (reduce
    play-turn
    {:scores {}
     :circle [0]
     :current 0}
    (map vector
         (cycle (range 1 (inc players)))
         (range 1 (inc last-marble)))))

(defn part1 [input]
  (let [[players last-marble] (parse-input input)]
    (-> (play-game players last-marble)
        (:scores)
        (vals)
        (sort)
        (last))))
