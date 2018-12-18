(ns day17
  (:require [clojure.string]))

(defn parse-coordinates [s]
  (let [[start end] (clojure.string/split s #"\.\.")
        end (if end end start)]
    [(Integer/parseInt start) (Integer/parseInt end)]))

(defn parse-line [line]
  (let [[_ sym1 val1 sym2 val2] (re-find #"([xy])=([^,]+), ([xy])=(.+)" line)]
    {(keyword sym1) (parse-coordinates val1) (keyword sym2) (parse-coordinates val2)}))

(defn parse-input [input]
  (let [tiles (reduce
                (fn [tiles {[x-min x-max] :x [y-min y-max] :y}]
                  (merge
                    tiles
                    (zipmap (for [y (range y-min (inc y-max))
                                  x (range x-min (inc x-max))]
                              [y x])
                            (repeat \#))))
                {}
                (map parse-line (clojure.string/split-lines input)))]
    {:tiles tiles
     :max-y (reduce max (map first (keys tiles)))}))

(def velocity->char {[1 0] \v
                     [0 -1] \<
                     [0 1] \>})

(defn print-state [{:keys [tiles max-y]} position velocity]
  (let [min-x (reduce min (map second (keys tiles)))
        max-x (reduce max (map second (keys tiles)))]
    (doseq [y (range 0 (inc max-y))]
      (println (reduce str (map (fn [x]
                                  (if (= [y x] position)
                                    (velocity->char velocity)
                                    (get tiles [y x] \.)))

                                (range min-x (inc max-x))))))))


(defn can-move? [{:keys [tiles]} position velocity]
  (nil? (get tiles (mapv + position velocity))))

(defn can-drop? [state position]
  (can-move? state position [1 0]))

(defn drop-water [state position]
  (loop [state state
         position position
         velocity [1 0]
         horizontal #{}]
    (print-state state position velocity)
    (println)
    (Thread/sleep 1000)
    (cond
      (= velocity [1 0])
      (if (can-drop? state position)
        (recur state (mapv + position velocity) velocity #{})
        (recur state position [0 1] #{}))

      (= velocity [0 1])
      (cond
        (can-drop? state position) (recur state position [1 0] #{})
        (can-move? state position velocity) (recur state (mapv + position velocity) velocity (conj horizontal position))
        :else (recur state position [0 -1] #{}))

      (= velocity [0 -1])
      (cond
        (can-drop? state position) (recur state position [1 0] #{})
        (can-move? state position velocity) (recur state (mapv + position velocity) velocity (conj horizontal position))
        :else (update state :tiles merge (zipmap (conj horizontal position) (repeat \~)))))))


(defn game [state]
  (let [state (drop-water state [0 500])]
    (print-state state nil nil)
    (println)
    (Thread/sleep 5000)
    (recur state)))

(def small-input "x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504")
