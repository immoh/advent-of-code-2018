(ns day18
  (:require [clojure.set]
            [clojure.string]))

(defn neighbors [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= dx dy 0)]
    [(+ x dx) (+ y dy)]))

(defn neighbor-freqs [area p]
  (frequencies (keep #(get area %) (neighbors p))))

(defn next-contents [area p c]
  (let [neighbor-freqs (neighbor-freqs area p)]
    (case c
      \. (if (>= (get neighbor-freqs \| 0) 3) \| \.)
      \| (if (>= (get neighbor-freqs \# 0) 3) \# \|)
      \# (if (and (pos? (get neighbor-freqs \# 0))
                  (pos? (get neighbor-freqs \| 0)))
           \#
           \.))))

(defn print-area [area]
  (doseq [x (range (inc (reduce max (map first (keys area)))))]
    (println (reduce str (map #(get area [x %]) (range (inc (reduce max (map second (keys area)))))))))
  (println))

(defn next-generation [area]
  (into {} (map (fn [[p c]]
                  [p (next-contents area p c)])
                area)))

(defn calculate-value [area]
  (let [freqs (frequencies (vals area))]
    (* (get freqs \| 0) (get freqs \# 0))))

(defn parse-input [input]
  (let [lines (mapv vec (clojure.string/split-lines input))]
    (reduce
      (fn [m p]
        (assoc m p (get-in lines p)))
      {}
      (for [x (range (count lines))
            y (range (count (first lines)))]
        [x y]))))

(defn part1 [input]
  (calculate-value (nth (iterate next-generation (parse-input input)) 10)))

(defn find-cycle [area]
  (loop [i 0
         area area
         seen {}]
    (if-let [n (get seen area)]
      [n i seen]
      (recur (inc i) (next-generation area) (assoc seen area i)))))

(defn part2 [input]
  (let [[first-occurence second-occurence areas] (find-cycle (parse-input input))]
    (calculate-value (get (clojure.set/map-invert areas)
                          (+ first-occurence (mod (- 1000000000 first-occurence) (- second-occurence first-occurence)))))))
