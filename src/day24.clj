(ns day24
  (:require [clojure.string]))

(defn parse-spec [spec]
  (let [[_ units] (re-find #"(\d+) units" spec)
        [_ hit-points] (re-find #"(\d+) hit points" spec)
        [_ weak-to] (re-find #"weak to ([^\);]+)" spec)
        [_ immune-to] (re-find #"immune to ([^\);]+)" spec)
        [_ damage-power damage-type] (re-find #"attack that does (\d+) ([^ ]+) damage" spec)
        [_ initiative] (re-find #"initiative (\d+)" spec)]
    {:units        (Integer/parseInt units)
     :hit-points   (Integer/parseInt hit-points)
     :weak-to      (set (when weak-to (clojure.string/split weak-to #", ")))
     :immune-to    (set (when immune-to (clojure.string/split immune-to #", ")))
     :damage-power (Integer/parseInt damage-power)
     :damage-type  damage-type
     :initiative   (Integer/parseInt initiative)}))

(defn parse-group [type i spec]
  (merge {:id (str type i)
          :type type}
         (parse-spec spec)))

(defn parse-group-type [[title & groups]]
  (map-indexed (partial parse-group (subs title 0 (dec (count title)))) groups))

(defn parse-input [input]
  (let [[group-type1 [_ & group-type2]] (split-with #(not= "" %) (clojure.string/split-lines input))]
    (mapcat parse-group-type [group-type1 group-type2])))

(defn effective-power [{:keys [units damage-power]}]
  (* units damage-power))

(defn damage [{:keys [damage-type] :as attacker} {:keys [immune-to weak-to]}]
  (* (cond
       (contains? immune-to damage-type) 0
       (contains? weak-to damage-type) 2
       :else 1)
     (effective-power attacker)))

(defn choose-target [attacker groups]
  (let [target (last (sort-by (juxt (partial damage attacker) effective-power) groups))]
    (when (pos? (damage attacker target))
      target)))

(defn choose-targets [groups]
  (:targets (reduce
              (fn [{:keys [targets available] :as acc} attacker]
                (if-let [target (choose-target attacker (filter #(not= (:type %) (:type attacker)) available))]
                  {:targets   (assoc targets (:id attacker) (:id target))
                   :available (remove #{target} available)}
                  acc))
              {:targets   {}
               :available groups}
              (reverse (sort-by (juxt effective-power :initiative) groups)))))

(defn attack [groups-alive attacker-id target-id]
  (let [attacker (get groups-alive attacker-id)
        {:keys [hit-points units] :as target } (get groups-alive target-id)
        damage (damage attacker target)
        remaining-units (- units (Math/floorDiv ^int damage ^int hit-points))]
    (if (pos? remaining-units)
      (assoc-in groups-alive [target-id :units] remaining-units)
      (dissoc groups-alive target-id))))

(defn round [groups-alive]
  (let [targets (choose-targets (vals groups-alive))]
    (reduce
      (fn [groups-alive attacker-id]
        (let [target-id (get targets attacker-id)]
          (if (and target-id (get groups-alive attacker-id))
            (attack groups-alive attacker-id target-id)
            groups-alive)))
      groups-alive
      (map :id (reverse (sort-by :initiative (vals groups-alive)))))))

(defn game [groups]
  (loop [groups-alive (zipmap (map :id groups) groups)]
    (prn (map :id (vals groups-alive)))
    (if (= (count (distinct (map :type (vals groups-alive)))) 1)
      groups-alive
      (recur (round groups-alive)))))

(defn calculate-units [groups]
  (reduce + (map :units (vals groups))))

(defn part1 [input]
  (calculate-units (game (parse-input input))))

(defn boost [groups type boost]
  (map #(if (= type (:type %))
          (update % :damage-power + boost)
          %)
       groups))

(defn part2 [input]
  (calculate-units (game (boost (parse-input input) "Immune System" 49))))
