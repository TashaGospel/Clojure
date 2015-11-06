(ns testing)

; (def frame (java.awt.Frame.))
; (.setVisible frame)
; (.setSize frame (java.awt.Dimension 200 200))
; (def gfx (.getGraphics frame))

(defn f-values [f max-x max-y]
  (for [x (range max-x) y (range max-y)]
    [x y (rem (f x y) 256)]))

(defn clear [gfx]
  (.clearRect gfx 0 0 300 300))

(defn draw [f gfx size-x size-y]
  (clear gfx)
  (doseq [[x y xor] (f-values f size-x size-y)]
    (.setColor gfx (java.awt.Color. xor xor xor))
    (.fillRect gfx x y 1 1)))

(defn primes
  "Returns a lazy sequence of the primes from 2 to infinity.
  Currently overflows at around 1000 primes.
  To be fixed."
  {:tag clojure.lang.LazySeq}
  []
  (letfn [(sieve [coll]
            (let [x (first coll)]
              (cons x
                    (lazy-seq (sieve (remove #(zero? (mod % x)) (next coll)))))))]
    (sieve (iterate inc 2))))

(defn index [coll]
  (cond
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map-indexed vector coll)))

(defn pos [pred coll]
  (for [[idx v] (index coll) :when (pred v)] idx))

(defn xconj [t v]
  (cond
    (nil? t) {:val v :L nil :R nil}
    (< v (:val t)) {:val (:val t)
                    :L (xconj (:L t) v)
                    :R (:R t)}
    :else {:val (:val t)
           :L (:L t)
           :R (xconj (:R t) v)}))

(defn xseq [t]
  (if t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))

(defn rand-ints [n]
  (repeatedly n #(rand-int n)))

(defn sort-parts [work]
  (lazy-seq
    (loop [[part & parts] work]
      (if-let [[pivot & xs] (seq part)]
        (let [smaller? #(< % pivot)]
          (recur (list* (filter smaller? xs)
                        pivot
                        (remove smaller? xs)
                        parts)))
        (if-let [[x & parts] parts]
          (cons x (sort-parts parts)))))))

(defn qsort [coll]
  (sort-parts (list coll)))
