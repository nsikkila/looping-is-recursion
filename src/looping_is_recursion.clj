(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
        (if (zero? exp)
          acc
          (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (not (= (count seq1) (count seq2)))
     false
   (and (empty? seq1) (empty? seq2))
     true
   (not (= (first seq1) (first seq2)))
    false
   :else
    (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  ":(")

(defn avg [a-seq]
  -1)

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

