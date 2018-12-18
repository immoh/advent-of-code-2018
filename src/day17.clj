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

(defn print-tiles [{:keys [tiles max-y]}]
  (let [min-x (reduce min (map second (keys tiles)))
        max-x (reduce max (map second (keys tiles)))]
    (doseq [y (range 0 (inc max-y))]
      (println (reduce str (map #(get tiles [y %] \.) (range min-x (inc max-x))))))))


(defn prompt! [msg]
  (locking *out*
    (loop []
      (print (str msg " [y/N] "))
      (flush)
      (case (clojure.string/lower-case (read-line))
        ("y" "yes") true
        ("" "n" "no") false
        (recur)))))

(defn drop-water [state position]
  (loop [state state
         position position
         velocity [1 0]]))
