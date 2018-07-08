(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
  (helper 1 base exp)))


(defn last-element [a-seq]
  (let [helper (fn [prev a-seq]
                 (if (empty? a-seq)
                   prev
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2))  true
    (and (not (empty? seq1)) (empty? seq2)) false
    (and (empty? seq1) (not (empty? seq2))) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         pred pred
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) index
      :else (recur (inc index) pred (rest a-seq)))))

(defn avg [a-seq]
  (loop [n 0
         sum 0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ sum n)
      (recur (inc n) (+ sum (first a-seq)) (rest a-seq)))))

(defn parity [a-seq]
  (loop [result #{}
         a-seq a-seq]
    (if (empty? a-seq) result
        (recur (if (contains? result (first a-seq))
                 (disj result (first a-seq)) 
                 (conj result (first a-seq)))
               (rest a-seq)))))

(defn fast-fibo [n]
  (loop [Fn-1 0
         Fn 1
         n n]
    (if
        (<= n 0) Fn-1
        (recur Fn (+ Fn-1 Fn) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [new-seq []
         elements #{}
         x (first a-seq)
         xs (rest a-seq)]
    (cond
      (elements x) new-seq
      (empty? xs) (conj new-seq x)
      :else (recur (conj new-seq x)
                   (conj elements x)
                   (first xs)
                   (rest xs)))))
