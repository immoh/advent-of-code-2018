(ns day04
  (:require [clojure.string]))

(defn parse-date [line]
  (re-find #"\d{4}-\d{2}-\d{2}" line))

(defn parse-minute [line]
  (Integer/parseInt (second (re-find #"\d{2}:(\d{2})" line))))

(defn parse-sleep-lines [sleep-lines]
  (for [[falls-asleep wakes-up] (partition-all 2 sleep-lines)]
    {:date (parse-date falls-asleep)
     :falls-asleep (parse-minute falls-asleep)
     :wakes-up     (parse-minute wakes-up)}))

(defn parse-shift [[[begins-line] sleep-lines]]
  {:guard-id (Integer/parseInt (second (re-find #"#(\d+)" begins-line)))
   :sleeps   (parse-sleep-lines sleep-lines)})

(defn combine-days [guards]
  (map (fn [[guard-id entries]]
         {:guard-id guard-id
          :sleeps   (sort-by :date (mapcat :sleeps entries))})
       (group-by :guard-id guards)))

(defn parse-input [input]
  (->> (clojure.string/split-lines input)
       (sort)
       (partition-by #(clojure.string/includes? % "begins shift"))
       (partition-all 2)
       (map parse-shift)
       (combine-days)))

(defn calculate-total-sleep [sleeps]
  (reduce + (map (fn [{:keys [falls-asleep wakes-up]}]
                   (- wakes-up falls-asleep))
                 sleeps)))

(defn add-total-sleep [{:keys [sleeps] :as guard}]
  (assoc guard :total-sleep (calculate-total-sleep sleeps)))

(defn find-max-by [f coll]
  (:max-item (reduce
               (fn [{:keys [max-value] :as current-max} item]
                 (let [value (f item)]
                   (if (> value max-value)
                     {:max-value value
                      :max-item  item}
                     current-max)))
               {:max-value (f (first coll))
                :max-item  (first coll)}
               (rest coll))))

(defn sleeps-by-minute [sleeps]
  (reduce
    (fn [m {:keys [falls-asleep wakes-up]}]
      (merge-with + m (zipmap (range falls-asleep wakes-up) (repeat 1))))
    {}
    sleeps))

(defn add-most-sleeps-minute [{:keys [sleeps] :as guard}]
  (assoc guard :most-sleeps-minute (find-max-by val (sleeps-by-minute sleeps))))

(defn part1 [input]
  (let [{:keys [guard-id most-sleeps-minute]} (->> (parse-input input)
                                                   (map add-total-sleep)
                                                   (find-max-by :total-sleep)
                                                   (add-most-sleeps-minute))]
    (* guard-id (key most-sleeps-minute))))

(defn part2 [input]
  (let [{:keys [guard-id most-sleeps-minute]} (->> (parse-input input)
                                                   (map add-most-sleeps-minute)
                                                   (find-max-by (comp val :most-sleeps-minute)))]
    (* guard-id (key most-sleeps-minute))))
