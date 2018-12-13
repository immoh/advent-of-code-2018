(ns day13
  (:require [clojure.string]))

(defn get-track-component [c]
  (case c
    \- :horizontal
    \| :vertical
    \/ :curve-right
    \\ :curve-left
    \+ :intersection
    \^ :vertical
    \v :vertical
    \< :horizontal
    \> :horizontal
    nil))

(defn get-cart [c location]
  (when-let [velocity (case c
                        \^ [-1 0]
                        \v [1 0]
                        \< [0 -1]
                        \> [0 1]
                        nil)]
    {:location location :velocity velocity :next-turn :left}))

(defn parse-input [input]
  (let [lines (mapv vec (vec (clojure.string/split-lines input)))]
    (reduce
      (fn [m location]
        (let [c (get-in lines location)
              track-component (get-track-component c)
              cart (get-cart c location)]
          (cond->
            m
            track-component (assoc-in [:track location] track-component)
            cart (update :carts conj cart))))
      {:track {}
       :carts []}
      (for [x (range (count lines))
            y (range (count (first lines)))]
        [x y]))))

(def new-next-turn {:left     :straight
                    :straight :right
                    :right    :left})

(defmulti handle-track-component (fn [track_component _] track_component))

(defmethod handle-track-component :intersection [_ {:keys [velocity next-turn]}]
  {:velocity (get-in {[-1 0] {:left     [0 -1]
                              :straight [-1 0]
                              :right    [0 1]}
                      [1 0]  {:left     [0 1]
                              :straight [1 0]
                              :right    [0 -1]}
                      [0 -1] {:left     [1 0]
                              :straight [0 -1]
                              :right    [-1 0]}
                      [0 1]  {:left     [-1 0]
                              :straight [0 1]
                              :right    [1 0]}}
                     [velocity next-turn])
   :next-turn (new-next-turn next-turn)})

(defmethod handle-track-component :curve-left [_ {:keys [velocity]}]
  {:velocity (case velocity
               [-1 0] [0 -1]
               [1 0] [0 1]
               [0 -1] [-1 0]
               [0 1] [1 0])})

(defmethod handle-track-component :curve-right [_ {:keys [velocity]}]
  {:velocity (case velocity
               [-1 0] [0 1]
               [1 0] [0 -1]
               [0 -1] [1 0]
               [0 1] [-1 0])})

(defmethod handle-track-component :default [_ _])

(defn update-cart [cart new-location track-component]
  (merge cart
         {:location new-location}
         (handle-track-component track-component cart)))

(defn move-cart [track carts cart-index]
  (if-let [{:keys [location velocity]} (get carts cart-index)]
    (let [new-location (mapv + location velocity)]
      (if (some (comp #{new-location} :location) carts)
        [:collision new-location]
        [:new-carts (update carts cart-index update-cart new-location (get track new-location))]))
    [:new-carts carts]))

(defn tick [track carts {:keys [remove-on-collision?]}]
  (loop [carts (vec (sort-by (comp vec reverse :location) carts))
         i 0]
    (if (= i (count carts))
      [:new-carts (vec (keep identity carts))]
      (let [[result-type data :as result] (move-cart track carts i)]
        (if (= result-type :collision)
          (if remove-on-collision?
            (recur (mapv (fn [{:keys [location] :as cart}]
                           (when (not= data location)
                             cart))
                        (assoc carts i nil))
                   (inc i))
            result)
          (recur data (inc i)))))))

(defn tick-until-collision [{:keys [track carts]}]
  (loop [carts carts]
    (let [[result-type data] (tick track carts {:remove-on-collision? false})]
      (if (= result-type :collision)
        data
        (recur data)))))

(defn part1 [input]
  (->> (parse-input input)
       (tick-until-collision)
       (reverse)
       (clojure.string/join ",")))

(defn tick-until-one-left [{:keys [track carts]}]
  (loop [carts carts]
    (let [[_ carts] (tick track carts {:remove-on-collision? true})]
      (if (= (count carts) 1)
        (:location (first carts))
        (recur carts)))))

(defn part2 [input]
  (->> (parse-input input)
       (tick-until-one-left)
       (reverse)
       (clojure.string/join ",")))
