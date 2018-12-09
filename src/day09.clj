(ns day09
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

(defn ->circle [value]
  (let [node (->Node value nil nil)]
    (set-prev-node! node node)
    (set-next-node! node node)
    node))

(defn add-value-to-circle [circle value]
  (let [node (get-next-node circle)
        next-node (get-next-node node)
        new-node (->Node value node next-node)]
    (set-next-node! node new-node)
    (set-prev-node! next-node new-node)))

(defn remove-node [node]
  (let [prev-node (get-prev-node node)
        next-node (get-next-node node)]
    (set-next-node! prev-node next-node)
    (set-prev-node! next-node prev-node)
    next-node))

(defn parse-input [input]
  (map #(Integer/parseInt %) (re-seq #"\d+" input)))

(defn play-turn [{:keys [scores circle]} [player marble]]
  (if (zero? (mod marble 23))
    (let [removable-node (nth (iterate get-prev-node circle) 7)]
      {:circle  (remove-node removable-node)
       :scores  (update scores player (fnil + 0) marble (get-value removable-node))})
    {:circle  (add-value-to-circle circle marble)
     :scores  scores}))

(defn play-game [players last-marble]
  (reduce
    play-turn
    {:scores {}
     :circle (->circle 0)}
    (map vector
         (cycle (range 1 (inc players)))
         (range 1 (inc last-marble)))))

(defn solve [input]
  (let [[players last-marble] (parse-input input)]
    (-> (play-game players last-marble)
        (:scores)
        (vals)
        (sort)
        (last))))
