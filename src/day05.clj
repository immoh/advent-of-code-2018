(ns day05
  (:require [clojure.string]))

(defn unit-types [polymer]
  (distinct (clojure.string/lower-case polymer)))

(defn create-reactions [unit-types]
  (mapcat (fn [c]
            [(str (clojure.string/lower-case c) (clojure.string/upper-case c))
             (str (clojure.string/upper-case c) (clojure.string/lower-case c))])
          unit-types))

(defn remove-reactions [polymer reactions]
  (reduce
    (fn [polymer reaction]
      (clojure.string/replace polymer reaction ""))
    polymer
    reactions))

(defn react [polymer]
  (let [reactions (create-reactions (unit-types polymer))]
    (loop [polymer polymer]
      (let [new-polymer (remove-reactions polymer reactions)]
        (if (= polymer new-polymer)
          (count polymer)
          (recur new-polymer))))))

(defn part1 [input]
  (react input))

(defn remove-type [polymer unit-type]
  (-> polymer
      (clojure.string/replace (str (clojure.string/lower-case unit-type)) "")
      (clojure.string/replace (str (clojure.string/upper-case unit-type)) "")))

(defn part2 [input]
  (apply min (map (fn [removable-type]
                    (react (remove-type input removable-type)))
                  (unit-types input))))
