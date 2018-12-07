(ns day07
  (:require [clojure.string]))

(defn parse-line [line]
  (filter #(= 1 (count %)) (clojure.string/split line #" ")))

(defn parse-input [input]
  (map parse-line (clojure.string/split-lines input)))

(defn create-dependency-map [requirements]
  (reduce
    (fn [deps-map [required-task task]]
      (-> deps-map
          (update task (fnil conj #{}) required-task)
          (update required-task (fnil identity #{}))))
    {}
    requirements))

(defn find-next-task [deps-map]
  (first (sort (keys (filter (comp empty? val) deps-map)))))

(defn remove-task [deps-map task]
  (-> (zipmap (keys deps-map)
              (map #(disj % task) (vals deps-map)))
      (dissoc task)))

(defn execute [deps-map]
  (loop [deps-map deps-map
         sequence ""]
    (if (empty? deps-map)
      sequence
      (let [task (find-next-task deps-map)]
        (recur (remove-task deps-map task)
               (str sequence task))))))

(defn part1 [input]
  (-> (parse-input input)
      (create-dependency-map)
      (execute)))

(defn allocate-task [workers task]
  (assoc workers (first (keep-indexed (fn [i worker]
                                        (when (= worker :idle)
                                          i))
                                      workers))
                 {:task task
                  :remaining (- (int (first task)) 4)}))

(defn find-next-task-to-finish [workers]
  (first (sort-by :remaining (filter :remaining workers))))

(defn finish-task [workers task t]
  (mapv (fn [worker]
          (if (or (= :idle worker) (= task (:task worker)))
            :idle
            (update worker :remaining - t)))
        workers))

(defn execute-in-parallel [deps-map worker-count]
  (loop [deps-map deps-map
         workers (vec (repeat worker-count :idle))
         t 0]
    (let [task (find-next-task deps-map)]
      (cond
        (and (empty? deps-map) (every? #{:idle} workers))
        t

        (and (some #{:idle} workers) task)
        (recur (dissoc deps-map task)
               (allocate-task workers task)
               t)

        :else
        (let [{:keys [task remaining]} (find-next-task-to-finish workers)]
          (recur (remove-task deps-map task)
                 (finish-task workers task remaining)
                 (+ t remaining)))))))

(defn part2 [input]
  (-> (parse-input input)
      (create-dependency-map)
      (execute-in-parallel 5)))
