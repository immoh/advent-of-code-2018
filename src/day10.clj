(ns day10
  (:require [clojure.string]))

(defn parse-coordinates [coordinates]
  (mapv (comp #(Integer/parseInt %) clojure.string/trim)
        (clojure.string/split (subs coordinates 1 (dec (count coordinates))) #",")))

(defn parse-line [line]
  (zipmap [:position :velocity]
          (map parse-coordinates (re-seq #"<[^>]+>" line))))

(defn parse-input [input]
  (map parse-line (clojure.string/split-lines input)))

(defn expand [xs]
  (distinct (mapcat (fn [x] [(dec x) x (inc x)]) xs)))

(defn visualize [points]
  (let [xs (sort (expand (map (comp first :position) points)))
        has-point? (set (map :position points))]
    (doseq [y (sort (expand (map (comp second :position) points)))]
      (println (reduce str (map (fn [x]
                                  (if (has-point? [x y]) "#" "."))
                                xs))))))

(defn evolve [points]
  (map (fn [{:keys [position velocity] :as point}]
         (assoc point :position (mapv + position velocity)))
       points))

(defn size [points]
  (let [min-x (reduce min (map (comp first :position) points))
        max-x (reduce max (map (comp first :position) points))
        min-y (reduce min (map (comp second :position) points))
        max-y (reduce max (map (comp second :position) points))]
    (* (- max-x min-x) (- max-y min-y))))

(defn find-local-minimum [points]
  (loop [t 0
         points points
         min-size (size points)]
    (let [next-points (evolve points)
          next-size (size next-points)]
      (if (> next-size min-size)
        {:time t :points points}
        (recur (inc t) next-points next-size)))))

(defn part1 [input]
  (visualize (:points (find-local-minimum (parse-input input)))))

(defn part2 [input]
  (:time (find-local-minimum (parse-input input))))
