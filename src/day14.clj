(ns day14
  (:require [clojure.string]))

(defprotocol INode
  (get-value [_])
  (get-next-node [_])
  (set-next-node! [_ ^INode node])
  (get-prev-node [_])
  (set-prev-node! [_ ^INode node]))

(deftype Node [value ^:volatile-mutable prev ^:volatile-mutable next]
  INode
  (get-value [_] value)
  (get-next-node [_] next)
  (set-next-node! [_ node] (set! next node))
  (get-prev-node [_] prev)
  (set-prev-node! [_ node] (set! prev node)))

(defn create-starting-state []
  (let [node1 (->Node 3 nil nil)
        node2 (->Node 7 node1 node1)]
    (set-prev-node! node1 node2)
    (set-next-node! node1 node2)
    {:elves        [node1 node2]
     :last-recipe  node2
     :recipe-count 2}))

(defn insert-value-after [node value]
  (let [next-node (get-next-node node)
        node-to-insert (->Node value node next-node)]
    (set-next-node! node node-to-insert)
    (set-prev-node! next-node node-to-insert)
    node-to-insert))

(defn move-forward [node n]
  (nth (iterate get-next-node node) n))

(defn move-backward [node n]
  (nth (iterate get-prev-node node) n))

(defn take-nodes [node n]
  (take n (iterate get-next-node node)))

(defn digits [n]
  (if (< n 10)
    [n]
    [1 (mod n 10)]))

(defn execute-round [{:keys [elves last-recipe recipe-count]}]
  (let [current-recipes (mapv get-value elves)
        new-recipes (digits (reduce + current-recipes))]
    {:last-recipe  (reduce insert-value-after last-recipe new-recipes)
     :elves        (map (fn [elf current-recipe]
                          (move-forward elf (inc current-recipe)))
                        elves
                        current-recipes)
     :recipe-count (+ recipe-count (count new-recipes))}))

(defn min-recipes [recipes]
  (first (drop-while #(< (:recipe-count %) recipes)
                     (iterate execute-round (create-starting-state)))))

(defn part1 [recipes]
  (let [{:keys [last-recipe recipe-count]} (min-recipes (+ recipes 10))]
    (-> (move-backward last-recipe (- recipe-count recipes 1))
        (take-nodes 10)
        (->> (map get-value))
        (clojure.string/join))))

(defn recipes-until-seq [recipe-seq]
  (first (drop-while (fn [{:keys [last-recipe]}]
                       (not (contains? (set [{(-> last-recipe
                                                  (move-backward (dec (count recipe-seq)))
                                                  (take-nodes (count recipe-seq))
                                                  (->> (map get-value)))
                                              (-> last-recipe
                                                  (move-backward (count recipe-seq))
                                                  (take-nodes (count recipe-seq))
                                                  (->> (map get-value)))}])
                                       recipe-seq)))
                     (iterate execute-round (create-starting-state)))))

(defn parse-input [input]
  (map #(- (int %) 48) input))

(defn part2 [recipe-seq]
  (-> (recipes-until-seq (parse-input recipe-seq))
      (:recipe-count)
      (- (count recipe-seq))))
