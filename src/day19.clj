(ns day19
  (:require [clojure.string]))

(defn create-op [operand immediate?]
  (fn [registers a b c]
    (assoc registers c (operand (registers a) (if immediate? b (registers b))))))

(def addr (create-op + false))
(def addi (create-op + true))
(def mulr (create-op * false))
(def muli (create-op * true))
(def banr (create-op bit-and false))
(def bani (create-op bit-and true))
(def borr (create-op bit-or false))
(def bori (create-op bit-or true))

(defn setr [registers a _ c]
  (assoc registers c (registers a)))

(defn seti [registers a _ c]
  (assoc registers c a))

(defn gtir [registers a b c]
  (assoc registers c (if (> a (registers b)) 1 0)))

(defn gtri [registers a b c]
  (assoc registers c (if (> (registers a) b) 1 0)))

(defn gtrr [registers a b c]
  (assoc registers c (if (> (registers a) (registers b)) 1 0)))

(defn eqir [registers a b c]
  (assoc registers c (if (= a (registers b)) 1 0)))

(defn eqri [registers a b c]
  (assoc registers c (if (= (registers a) b) 1 0)))

(defn eqrr [registers a b c]
  (assoc registers c (if (= (registers a) (registers b)) 1 0)))

(def ops (zipmap (map name '[addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr])
                 [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr]))

(defn parse-instruction [line]
  (let [[op & args] (clojure.string/split line #" ")]
    {:op op :args (map #(Integer/parseInt %) args)}))

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)]
    {:instruction-pointer (Integer/parseInt (second (clojure.string/split (first lines) #" ")))
     :instructions        (into {} (map-indexed (fn [i line]
                                                  [i (parse-instruction line)])
                                                (rest lines)))}))

(defn execute [{:keys [instruction-pointer instructions]} registers]
  (loop [registers registers
         ip-value  0]
    (if-let [{:keys [op args]} (get instructions ip-value)]
      (let [registers (apply (get ops op) (assoc registers instruction-pointer ip-value) args)]
        (recur registers (inc (get registers instruction-pointer))))
      registers)))

(defn part1 [input]
  (first (execute (parse-input input) [0 0 0 0 0 0])))
