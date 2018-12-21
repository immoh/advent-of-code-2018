(ns day20
  (:require [clojure.string]))

(declare regex->tree)

(defn split-branches [s]
  (let [{:keys [branches buffer]} (reduce
                                    (fn [{:keys [buffer level branches] :as m} c]
                                      (case c
                                        \| (merge {:level level}
                                                  (if (zero? level)
                                                    {:branches (conj branches buffer)
                                                     :buffer   ""}
                                                    {:branches branches
                                                     :buffer   (str buffer c)}))
                                        \( (merge {:level    (inc level)
                                                   :branches branches
                                                   :buffer   (str buffer c)})
                                        \) (merge {:level    (dec level)
                                                   :branches branches
                                                   :buffer   (str buffer c)})
                                        (update m :buffer str c)))
                                    {:level    0
                                     :branches []
                                     :buffer   ""}
                                    s)]
    (conj branches buffer)))

(defn find-branches [s]
  (let [{:keys [seqs buffer]} (reduce
                                (fn [{:keys [buffer level seqs] :as m} c]
                                  (case c
                                    \( (merge {:level (inc level)}
                                              (if (zero? level)
                                                {:seqs   (conj seqs buffer)
                                                 :buffer "("}
                                                {:seqs   seqs
                                                 :buffer (str buffer c)}))
                                    \) (merge {:level (dec level)}
                                              (if (= 1 level)
                                                {:seqs   (conj seqs (str buffer ")"))
                                                 :buffer ""}
                                                {:seqs   seqs
                                                 :buffer (str buffer c)}))
                                    (update m :buffer str c)))
                                {:level  0
                                 :seqs   []
                                 :buffer ""}
                                s)]
    (conj seqs buffer)))

(defn regex->tree [s]
  (cond
    (clojure.string/starts-with? s "(") (into [:or] (mapv regex->tree (split-branches (subs s 1 (dec (count s))))))
    (clojure.string/includes? s "(") (mapv regex->tree (find-branches s))
    :else s))

(defn expand-one [{:keys [string remaining]}]
  (if-let [node (first remaining)]
    (cond
      (string? node)
      [{:string (str string node) :remaining (rest remaining)}]

      (= (first node) :or)
      (mapv (fn [next-node]
              {:string string :remaining (into [next-node] (rest remaining))})
            (rest node))

      :else
      [{:string string :remaining (into node (rest remaining))}])
    [{:string string}]))


(def direction->delta {\N [1 0]
                       \S [-1 0]
                       \W [0 1]
                       \E [0 -1]})

(defn expand-regex [s]
  (loop [expandables [{:string "" :remaining (regex->tree s)}]]
    (if (some :remaining expandables)
      (recur (mapcat expand-one expandables))
      (map :string expandables))))

(defn walk-path [grid location path]
  (if-let [c (first path)]
    (let [new-location (mapv + location (direction->delta c))]
      (recur (update grid location (fnil conj #{}) new-location)
             new-location
             (rest path)))
    grid))

(defn walk-paths [paths]
  (reduce
    (fn [grid path]
      (walk-path grid [0 0] path))
    {}
    paths))

(defn shortest-distances [grid]
  (loop [distances (-> (zipmap (keys grid) (repeat nil))
                       (assoc [0 0] 0))
         unvisited (-> (keys grid) (set) (disj [0 0]))
         current [0 0]]
    (if current
      (let [current-distance (get distances current)
            new-distances (reduce (fn [distances neighbor]
                                    (let [distance (get distances neighbor)]
                                      (assoc distances neighbor (if distance
                                                                  (min distance (inc current-distance))
                                                                  (inc current-distance)))))
                                  distances
                                  (get grid current))
            new-unvisited (disj unvisited current)]
        (recur new-distances
               new-unvisited
               (some->> (select-keys new-distances new-unvisited)
                        (filter val)
                        (sort-by val)
                        (first)
                        (key))))
      distances)))

(defn part1 [input]
  (-> (expand-regex (subs input 1 (dec (count input))))
      (walk-paths)
      (shortest-distances)
      (vals)
      (sort)
      (last)))
