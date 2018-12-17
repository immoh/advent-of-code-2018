(ns day16
  (:require [clojure.set]
            [clojure.string]))

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

(def ops [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr])

(defn op-matches? [register-before instruction register-after op]
  (= (apply op register-before (rest instruction)) register-after))

(defn matching-ops [register-before instruction register-after]
  (count (filter true? (map (partial op-matches? register-before instruction register-after) ops))))

(defn parse-register [register-line]
  (mapv #(Integer/parseInt %)
        (clojure.string/split (second (re-find #"\[([^\]]+)\]" register-line)) #", ")))

(defn parse-instruction [instruction-line]
  (map #(Integer/parseInt %) (clojure.string/split instruction-line #" ")))

(defn parse-case [[before-line instruction-line after-line]]
  {:register-before (parse-register before-line)
   :instruction     (parse-instruction instruction-line)
   :register-after  (parse-register after-line)})

(defn parse-input1 [input]
  (map parse-case (partition-all 4 (clojure.string/split-lines input))))

(defn part1 [input]
  (->> (parse-input1 input)
       (map (fn [{:keys [register-before instruction register-after]}]
              (matching-ops register-before instruction register-after)))
       (filter #(>= % 3))
       (count)))

(defn work-out-opcodes [cases]
  (loop [candidates (reduce
                      (fn [mapping {:keys [register-before instruction register-after]}]
                        (update mapping
                                (first instruction)
                                (fnil clojure.set/intersection (set ops))
                                (set (filter (partial op-matches? register-before instruction register-after) ops))))
                      {}
                      cases)
         mapping {}]
    (if (seq candidates)
      (let [entry (first (filter #(= 1 (count (val %))) candidates))
            code (key entry)
            op (first (val entry))]
        (recur (into {} (map (fn [[k v]] [k (disj v op)]) (dissoc candidates code)))
               (assoc mapping code op)))
      mapping)))

(defn parse-input2 [input]
  (map parse-instruction (clojure.string/split-lines input)))

(defn part2 [input1 input2]
  (let [code->op (work-out-opcodes (parse-input1 input1))]
    (first (reduce
             (fn [register [code & args]]
               (apply (code->op code) register args))
             [0 0 0 0]
             (parse-input2 input2)))))
