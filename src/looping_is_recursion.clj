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
  (loop [p pred sequ a-seq index 0]
    (cond
     (empty? sequ)
       nil
     (p (first sequ))
      index
     :else
      (recur p (rest sequ) (inc index)))))

(defn avg [a-seq]
  (loop [c 0 s a-seq sum 0]
    (if (empty? s)
      (/ sum c)
      (recur (inc c) (rest s) (+ sum (first s))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [s a-seq t #{}]
    (if (empty? s)
      t
      (recur (rest s) (toggle t (first s))))))

(defn fast-fibo [n]
    (loop [Fn 0 Fp 1 i 0]
      (if (= i n)
        Fn
        (recur (+ Fn Fp) Fn (inc i)))))


(defn cut-at-repetition [a-seq]
  [":("])

