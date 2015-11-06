(ns four-clojure)

; 4Clojure Problem 53
(defn longest-inc-subseq [coll]
  (let [sequences (reduce (fn [res cur]
                            (if (< (last (last res)) cur)
                              (concat (butlast res) (list (concat (last res) (list cur))))
                              (concat res (list (list cur)))))
                          (list (list (first coll))) (next coll))
        longest-seq (reduce (fn [res cur]
                              (if (< (count res) (count cur)) cur res))
                            () sequences)]
    (if (>= (count longest-seq) 2)
      longest-seq
      ())))

(defn better-longest-inc-subseq [coll]
  (->> 
    (range 2 (inc (count coll)))
    (mapcat #(partition % 1 coll))
    (filter #(apply < %))
    (cons [])
    (sort-by count >)
    first))

(defn my-comp [& fs]
  (let [fs (reverse fs)]
    (fn [& args]
      (loop [res (apply (first fs) args) fs (next fs)]
        (if fs
          (recur ((first fs) res) (next fs))
          res)))))

(defn my-reductions
  ([f coll] (my-reductions f (first coll) (next coll)))
  ([f init coll] (cons init
                       (lazy-seq
                         (when-not (nil? coll)
                           (my-reductions f (f init (first coll)) (next coll)))))))

(defn take-n-primes [cnt]
  (letfn [(prime? [x]
                  (loop [i (int (Math/sqrt x))]
                    (cond
                     (= 1 i) true
                     (= 0 (rem x i)) false
                     :else (recur (dec i)))))]
  (loop [cnt cnt x 2 res []]
    (cond
     (zero? cnt) res
     (prime? x) (recur (dec cnt) (inc x) (conj res x))
     :else (recur cnt (inc x) res)))))

(defn my-merge-with [f init & colls]
  (loop [init init colls colls]
    (if (seq colls)
      (recur (reduce (fn [res cur]
                       (let [x (get res (first cur))]
                       (assoc res (first cur)
                              (if x
                                (f (get res (first cur)) (last cur)) 
                                (last cur)))))
                     init (first colls))
             (rest colls))
      init)))
