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

(defn regex->tree* [s]
  (cond
    (clojure.string/starts-with? s "(") (into [:or] (map regex->tree* (split-branches (subs s 1 (dec (count s))))))
    (clojure.string/includes? s "(") (vec (remove #{""} (mapv regex->tree* (find-branches s))))
    :else s))

(defn regex->tree [s]
  (let [tree (regex->tree* s)]
    (if (string? tree)
      [tree]
      tree)))

(def direction->delta {\N [1 0]
                       \W [0 1]
                       \S [-1 0]
                       \E [0 -1]})

(defn walk-simple-path [grid location path]
  (if-let [c (first path)]
    (let [new-location (mapv + location (direction->delta c))]
      (recur (-> grid
                 (update location (fnil conj #{}) new-location)
                 (update new-location (fnil conj #{}) location))
             new-location
             (rest path)))
    [grid location]))

(defn walk-one [grid location tree]
  (if-let [node (first tree)]
    (cond
      (string? node)
      (let [[grid location] (walk-simple-path grid location node)]
        [grid [[location (rest tree)]]])

      (= (first node) :or)
      [grid (map (fn [next-node]
                   [location (into [next-node] (rest tree))])
                 (rest node))]

      :else
      [grid [[location (into node (rest tree))]]])
    [grid nil]))

(defn walk-paths [tree]
  (prn :walking-paths)
  (loop [grid {}
         walkables [[[0 0] tree]]]
    (prn :walking-paths (count grid) (count walkables))
    (if-let [[location element] (first walkables)]
      (let [[grid new-walkables] (walk-one grid location element)]
        (recur grid (into (rest walkables) new-walkables)))
      grid)))

(defn shortest-distances [grid]
  (prn :shortest-distances)
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
  (-> (regex->tree (subs input 1 (dec (count input))))
      (walk-paths)
      (shortest-distances)
      (vals)
      (sort)
      (last)))
