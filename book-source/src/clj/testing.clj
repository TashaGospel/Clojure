(ns testing
  (:import java.util.concurrent.Executors))

(defrecord TreeNode [val l r])

(defn xconj [t v]
  (cond
    (nil? t) (TreeNode. v nil nil)
    (< v (:val t)) (TreeNode. (:val t) (xconj (:l t) v) (:r t))
    :else (TreeNode. (:val t) (:l t) (xconj (:r t) v))))

(defn xseq [t]
  (if t
    (concat (xseq (:l t)) [(:val t)] (xseq (:r t)))))

(defprotocol FIXO
  (fixo-push [fixo value])
  (fixo-pop [fixo])
  (fixo-peek [fixo]))

(extend-type TreeNode
  FIXO
  (fixo-push [node value]
    (xconj node value))
  (fixo-peek [node]
    (if (:l node)
      (recur (:l node))
      (:val node)))
  (fixo-pop [node]
    (if (:l node)
      (TreeNode. (:val node) (fixo-pop (:l node)) (:r node))
      (:r node))))

(extend-type clojure.lang.IPersistentVector
  FIXO
  (fixo-push [vector value]
    (conj vector value))
  (fixo-pop [vector]
    (pop vector))
  (fixo-peek [vector]
    (peek vector)))

(extend-type nil
  FIXO
  (fixo-push [t v]
    (xconj t v)))

(defn fixo-into [c1 c2]
  (reduce fixo-push c1 c2))

(defn fixed-fixo
  ([limit] (fixed-fixo limit []))
  ([limit vector]
   (reify FIXO
     (fixo-push [this value]
       (if (< (count vector) limit)
         (fixed-fixo limit (conj vector value))
         this))
     (fixo-peek [_]
       (peek vector))
     (fixo-pop [_]
       (pop vector)))))

(defrecord Move [from to castle? promotion]
  Object
  (toString [_]
    (str "Move " from " to " to
         (if castle?
           " castle"
           (if promotion
             (str " promote to " promotion))))))

(defn build-move [& {:keys [from to castle? promotion]}]
  {:pre [from to]}
  (Move. from to castle? promotion))

(defn neighbors
  ([size yx] (neighbors [[0 -1] [0 1] [1 0] [-1 0]] size yx))
  ([delta size yx]
   (filter (fn [new-yx] (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %)) delta))))

(def initial-board [[:- :k :-]
                    [:- :- :-]
                    [:- :K :-]])

(defn board-map [f board]
  (vec (map #(vec (map f %)) board)))

(defn reset-board!
  "This is not a good idea! Don't do this!.
  Only for convenience at the REPL."
  []
  (def board (board-map ref initial-board))
  (def to-move (ref [[:K [2 1]] [:k [0 1]]]))
  (def num-moves (ref 0)))

(def king-moves
  (partial neighbors [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]] 3))

(defn good-move? [to enemy]
  (when (not= to enemy)
    to))

(defn choose-move
  [[[mover mpos] [_ enemy-pos]]]
  [mover (some #(good-move? % enemy-pos)
               (shuffle (king-moves mpos)))])

(defn place [from to] to)

(defn move-peice [[peice dest] [[_ src] _]]
  (commute (get-in board dest) place peice)
  (commute (get-in board src) place :-)
  (commute num-moves inc))

(defn update-to-move [move]
  (alter to-move #(vector (second %) move)))

(defn make-move []
  (dosync
    (let [move (choose-move @to-move)]
      (move-peice move @to-move)
      (update-to-move move))))

(defn ref-stress [r]
  (let [tries-count (atom 0)]
    (future
      (dosync (swap! tries-count inc)
              (Thread/sleep 200)
              @r)
      (println (format "r is %s, history is %s, after %s tries"
                       @r (.getHistoryCount r) @tries-count)))
    (dotimes [_ 500]
      (Thread/sleep 10)
      (dosync (alter r inc)))
    :done))

(def thread-pool
  (Executors/newFixedThreadPool
    (+ 2 (.availableProcessors (Runtime/getRuntime)))))

(defn dothreads!
  [f & {thread-count :threads
        exec-count :times
        :or {thread-count 1 exec-count 1}}]
  (dotimes [t thread-count]
    (.submit thread-pool
             #(dotimes [_ exec-count] (f)))))

(defn manipulable-memoize [f]
  (let [cache (atom {})]
    (with-meta
      {:cache cache}
      (fn [& args]
        (or (@cache args)
            (let [res (apply f args)]
              (swap! cache assoc args res)
              res))))))

(defn ^double asum-sq [^floats arr]
  (let [^floats sq (amap arr i ret
                         (* (aget arr i) (aget arr i)))]
    (areduce sq i ret 0 (+ ret (aget sq i)))))

(definterface ISliceable
  (slice [^long s ^long e])
  (^long sliceCount []))

(defprotocol Sliceable
  (slice [this s e])
  (sliceCount [this]))

(extend testing.ISliceable
  Sliceable
  {:slice (fn [this s e] (.slice this s e))
   :sliceCount (fn [this] (.sliceCount this))})

(defn calc-slice-count
  "Formula: (n + 1)! / k!(n + 1 - k)!, where k = 2, n = (count thing)."
  [thing]
  (let [n (count thing)]
    (/ (reduce * (range 3 (+ n 2)))
       (reduce * (range 1 n)))))

(extend-type String
  Sliceable
  (slice [this s e] (.substring this s (inc e)))
  (sliceCount [this] (calc-slice-count this)))

(defn zencat1 [x y]
  (loop [src y res x]
    (if src
      (recur (next src) (conj res (first src)))
      res)))

(defn zencat2 [x y]
  (loop [src y res (transient x)]
    (if src
      (recur (next src) (conj! res (first src)))
      (persistent! res))))

(defn seq1 [s]
  (lazy-seq
    (when-let [[x] (seq s)]
      (cons x (seq1 (rest s))))))

(def gcd (memoize
           (fn [x y]
             (cond
               (> x y) (recur (- x y) y)
               (< x y) (recur x (- y x))
               :else x))))

(set! *unchecked-math* true)
(defn factorial-a [^long original-x]
  (loop [res 1 x original-x]
    (if (> x 1)
      (recur (* res x) (dec x))
      res)))
(set! *unchecked-math* false)

(defn factorial-b [^double original-x]
  (loop [res 1.0 x original-x]
    (if (> x 1.0)
      (recur (* res x) (dec x))
      res)))

(defn factorial-c [^long original-x]
  (loop [res 1 x original-x]
    (if (> x 1)
      (recur (*' res x) (dec x))
      res)))

(defn empty-range? [start end step]
  (if (pos? step)
    (>= start end)
    (<= start end)))

(defn lazy-range [start end step]
  {:pre [(not (zero? step))]}
  (lazy-seq
    (if (empty-range? start end step)
      nil
      (cons start (lazy-range (+ start step)
                              end
                              step)))))
