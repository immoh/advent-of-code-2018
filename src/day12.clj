(ns day12
  (:require [clojure.string]))

(defn parse-initial-state [line]
  (second (clojure.string/split line #": ")))

(defn parse-rule [line]
  (zipmap [:if :then] (clojure.string/split line #" => ")))

(defn parse-rules [lines]
  (let [rules (map parse-rule lines)]
    (zipmap (map :if rules) (map :then rules))))

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)]
    {:initial (parse-initial-state (first lines))
     :rules   (parse-rules (drop 2 lines))}))

(defn evolve-pot [generation rules i]
  (get rules (subs generation (- i 2) (+ i 3))))

(defn evolve [rules generation]
  (reduce str (concat (take 2 generation)
                      (map (partial evolve-pot generation rules) (range 2 (- (count generation) 2)))
                      (take-last 2 generation))))

(defn count-empties [s]
  (count (take-while #{\.} s)))

(defn find-min-left-empty-pots [rules]
  (-> (filter #(= (val %) "#") rules)
      (keys)
      (sort)
      (reverse)
      (first)
      (count-empties)))

(defn find-min-right-empty-pots [rules]
  (->> (filter #(= (val %) "#") rules)
       (keys)
       (map clojure.string/reverse)
       (sort)
       (reverse)
       (first)
       (count-empties)))

(defn left-pad [generation lmin]
  (let [n (count (take-while #{\.} generation))]
    (if (< n lmin)
      (reduce str (concat (repeat (- lmin n) \.)
                          generation))
      generation)))

(defn right-pad [generation rmin]
  (let [n (count (take-while #{\.} (reverse generation)))]
    (if (< n rmin)
      (reduce str (concat generation
                          (repeat (- rmin n) \.)))
      generation)))

(defn pad-and-evolve [{:keys [initial rules]} generations]
  (let [lmin (find-min-left-empty-pots rules)
        rmin (find-min-right-empty-pots rules)]
    (nth
      (iterate
        (fn [{:keys [generation lpads]}]
          (let [left-pad-generation (left-pad generation lmin)]
            {:generation (evolve rules (right-pad left-pad-generation rmin))
             :lpads      (+ lpads (- (count left-pad-generation) (count generation)))}))
        {:generation initial
         :lpads      0})
      generations)))

(defn sum-plan-indices [{:keys [generation lpads]}]
  (reduce + (keep-indexed (fn [i pot]
                            (when (= pot \#)
                              (- i lpads)))
                          generation)))

(defn part1 [input]
  (-> (parse-input input)
      (pad-and-evolve 20)
      (sum-plan-indices)))

(defn part2 [input]
  (-> (parse-input input)
      (pad-and-evolve 5000)
      (sum-plan-indices)))
