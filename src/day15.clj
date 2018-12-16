(ns day15
  (:require [clojure.string])
  (:import (java.util UUID)))

(defn create-map-item [c]
  (case c
    \. {:type :free}
    \E {:type :elf :points 200 :id (UUID/randomUUID)}
    \G {:type :goblin :points 200 :id (UUID/randomUUID)}
    nil))

(defn parse-input [input]
  (let [lines (mapv vec (clojure.string/split-lines input))]
    (reduce
      (fn [m p]
        (if-let [item (create-map-item (get-in lines p))]
          (assoc m p item)
          m))
      {}
      (for [x (range (count lines))
            y (range (count (first lines)))]
        [x y]))))

(defn get-enemies [state enemy-type]
  (filter #(= (:type (val %)) enemy-type) state))

(defn distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^int (- x2 x1)) (Math/abs ^int (- y2 y1))))

(defn get-enemy-in-range [location enemies]
  #_(prn :get-enemy-in-range)
  #_(prn :location location :enemies enemies)
  (first (sort-by (juxt (comp :points val) key) (filter #(= (distance location (key %)) 1) enemies))))

(defn attack [state [location enemy]]
  (let [{:keys [points] :as updated-enemy} (update enemy :points - 3)]
    (assoc state location (if (<= points 0)
                            {:type :free}
                            updated-enemy))))

(defn get-neighbors [location possible-location?]
  (keep possible-location? (map (partial mapv + location) [[-1 0] [0 -1] [0 1] [1 0]])))

(defn update-paths [paths location path]
  (let [best-path (get paths location)
        new-path (conj path location)]
    (assoc paths location (cond
                            (not best-path) new-path
                            (< (count new-path) (count best-path)) new-path
                            (> (count new-path) (count best-path)) best-path
                            :else (first (sort [best-path new-path]))))))

(defn create-free-cell-set [state]
  (set (keys (filter #(= (:type (val %)) :free) state))))

(defn get-shortest-path-to-target [state from target]
  (let [free-cells (create-free-cell-set state)]
    (loop [paths (merge {from [] target nil} (zipmap free-cells (repeat nil)))
           unvisited free-cells
           current from]
      (when current
        (if (= current target)
          (get paths current)
          (let [neighbors (get-neighbors current free-cells)
                new-paths (reduce #(update-paths % %2 (get paths current)) paths neighbors)
                new-unvisited (disj unvisited current)]
            (recur new-paths
                   (disj unvisited current)
                   (some->> (select-keys new-paths new-unvisited)
                            (filter val)
                            (sort-by (juxt (comp count val) val))
                            (first)
                            (key)))))))))

(defn get-targets [state enemies]
  (let [free-cell? (create-free-cell-set state)]
    (mapcat #(get-neighbors (key %) free-cell?) enemies)))

(defn get-next-location [state location enemies]
  (->> (keep (partial get-shortest-path-to-target state location) (get-targets state enemies))
       (sort-by (juxt count last first))
       (ffirst)))

(defn move-player [state from to]
  (let [player (get state from)]
    (assoc state from {:type :free} to player)))

(defn turn [state location player]
  (let [enemy-type (if (= :elf (:type player)) :goblin :elf)
        enemies (get-enemies state enemy-type)]
    (if (seq enemies)
      (if-let [enemy-in-range (get-enemy-in-range location enemies)]
        {:state (attack state enemy-in-range)}
        (if-let [next-location (get-next-location state location enemies)]
          (if-let [enemy-in-range (get-enemy-in-range next-location enemies)]
            {:state (-> state (attack enemy-in-range) (move-player location next-location))}
            {:state (move-player state location next-location)})
          {:state state}))
      {:state state :round-incomplete? true})))

(defn get-players [state]
  (remove #(= (:type (val %)) :free) state))

(defn round [state]
  (loop [state state
         round-incomplete? false
         players (sort-by key (get-players state))]
    (if-let [player (first players)]
      (if (= (:id (val player)) (:id (get state (key player))))
        (let [{:keys [state round-incomplete?]} (turn state (key player) (val player))]
          (recur state round-incomplete? (rest players)))
        (recur state round-incomplete? (rest players)))
      {:state state :round-incomplete? round-incomplete?})))

(defn game-over? [state]
  (= 2 (count (distinct (map :type (vals state))))))

(defn print-state [state]
  (doseq [x (range (inc (reduce max (map first (keys state)))))]
    (println (reduce str (map (fn [y]
                                (case (:type (get state [x y]))
                                  :elf "E"
                                  :goblin "G"
                                  :free "."
                                  "#"))
                              (range (inc (reduce max (map second (keys state)))))))))
  (println (clojure.string/join " " (keep :points (vals state)))))

(defn calculate-points [rounds state]
  (prn {:rounds rounds :points (keep :points (vals state))})
  (* rounds (reduce + (keep :points (vals state)))))

(defn game [state]
  (loop [state state
         round-incomplete? false
         rounds 0]
    (println "Round" rounds)
    (print-state state)
    (if (game-over? state)
      (calculate-points (if round-incomplete? (dec rounds) rounds) state)
      (let [{:keys [state round-incomplete?]} (round state)]
        (recur state round-incomplete? (inc rounds))))))

(defn part1 [input]
  (game (parse-input input)))
