(ns day20
  (:require [clojure.string]))

"ENWWW(NEEE|SSE(EE|N))EWE(N|S)S"
:-> "EMWWW" "(NEEE|SSE(EE|N))" "EWE" "(N|S)" "S"

"(N|S)" :-> [:or ["N" "S"]]

"(NEEE|SSE(EE|N))" :-> [:or ["NEEE" "SSE(EE|N)"]]

(declare expand-regex)

(defn split-branches [s]
  s)

(defn find-branches [s]
  (:seqs (reduce
           (fn [{:keys [buffer level seqs] :as m} c]
             (case c
               \( (merge {:level (inc level)}
                         (if (zero? level)
                           {:seqs   (conj seqs buffer)
                            :buffer "("}
                           {:seqs   seqs
                            :buffer (str buffer c)}))
               \) (merge {:level (dec level)}
                         (if (= 1 level)
                           {:seqs   (conj seqs (str buffer ")"))
                            :buffer ""}
                           {:seqs   seqs
                            :buffer (str buffer c)}))
               (update m :buffer str c)))
           {:level  0
            :seqs   []
            :buffer ""}
           s)))

(defn expand-regex [s]
  (cond
    (clojure.string/starts-with? s "(") [:or (split-branches (subs s 1 (dec (count s))))]
    (clojure.string/includes? s "(") [:seq (find-branches s)]
    :else [:node s]))
