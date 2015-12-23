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

; Simpler but is O(2^n)
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

; An undirected graph has a Eulerian path if at most two vertices have odd degree
; and all vertices with non-zero degree belong to the same connected component.
; This implementation assumes all vertices are in the same connected component.
; Function connected-components should return a set of sets
; because it's more convenient.
(defn has-euler-path? [edges]
  (if (empty? (remove (fn [[a b]] (= a b)) edges))
    false
    (>= 2
        (count
          (filter odd?
                  (vals
                    ; Should have used frequencies here.
                    (reduce (fn [res [a b]]
                              (if (not= a b)
                                (assoc res
                                       a (inc (get res a 0))
                                       b (inc (get res b 0)))
                                res))
                            {} edges)))))))

(defn tic-tac-toe [board]
  (letfn [(win? [coll] 
            (let [coll (distinct coll)]
              (and (= 1 (count coll))
                   (not= :e (first coll))
                   (first coll))))]
    (some identity
      (apply list
             (win? (for [i (range 3)] (get-in board [i i])))
             (win? (for [i (range 3)] (get-in board [i (- 2 i)])))
             (flatten (for [i (range 3)] [(win? (map #(nth % i) board)) (win? (nth board i))]))))))

(defn levenshtein-distance [c1 c2]
  (let [n (inc (count c1)) m (inc (count c2))
        a (transient (vec (repeat n (vec (repeat m 0)))))]
    (dotimes [i n] (assoc! a i (assoc (get a i) 0 i)))
    (assoc! a 0 (vec (range m)))
    (dotimes [i n]
      (dotimes [j m]
        (when (and (pos? i) (pos? j))
          (assoc! a i (assoc (get a i) j
                             (if (= (get c1 (dec i)) (get c2 (dec j)))
                               (get-in a [(dec i) (dec j)])
                               (inc (min (get-in a [(dec i) j])
                                         (get-in a [i (dec j)])
                                         (get-in a [(dec i) (dec j)])))))))))
    (get-in a [(dec n) (dec m)])))
