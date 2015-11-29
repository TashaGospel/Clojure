(ns old-testing)

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

(def bearings [{:x  0 :y  1}    ; north
               {:x  1 :y  0}    ; east
               {:x  0 :y -1}    ; south
               {:x -1 :y  0}])  ; west

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn []
              (bot (+ x (:x (bearings bearing-num)))
                   (+ y (:y (bearings bearing-num)))
                   bearing-num))
   :turn-left (fn []
                (bot x y (mod (dec bearing-num) 4)))
   :turn-right (fn []
                (bot x y (mod (inc bearing-num) 4)))})

(defn elevator [commands]
  (letfn
    [(ff-open [[cur & more]]
       #(case cur
          :close (ff-close more)
          :done true
          false))
     (ff-close [[cur & more]]
       #(case cur
          :open (ff-open more)
          :up (sf-close more)
          :done true
          false))
     (sf-open [[cur & more]]
       #(case cur
          :close (sf-close more)
          :done true
          false))
     (sf-close [[cur & more]]
       #(case cur
          :open (sf-open more)
          :down (ff-close more)
          :done true
          false))]
    (trampoline ff-open commands)))

(defmacro do-until [test-expr res-expr & clauses]
  `(when ~test-expr
     ~res-expr
     ~(if (next clauses) `(do-until ~@clauses))))

(defmacro def-watch [name & value]
  `(do
     (def ~name ~@value)
     (add-watch #'~name
                :re-bind
                (fn [key# r# old-val# new-val#]
                  (println old-val# "->" new-val#)))))

(defmacro my-declare [& vars]
  (if (seq vars)
    `(do (def ~(first vars)) (my-declare ~@(rest vars)))))

(defmacro domain [name & body]
  `{:tag :domain
    :attrs {:name (str '~name)}
    :content [~@body]})

(declare handle-things grok-attrs grok-props)

(defmacro grouping [name & body]
  `{:tag :grouping
    :attrs {:name (str '~name)}
    :content [~@(handle-things body)]})

(defn handle-things [things]
  (for [t things]
    {:tag :thing
     :attrs (grok-attrs (take-while (complement vector?) t))
     :content (if-let [props (grok-props (drop-while (complement vector?) t))]
                [props]
                [])}))

(defn grok-attrs [thing]
  (into {:name (str (first thing))}
        (for [a (rest thing)]
          (cond
            (list? a) [:isa (str (second a))]
            (string? a) [:comment a]))))

(defn grok-props [thing]
  (if thing
    {:tag :properties
     :attrs nil
     :content (vec (for [p thing]
                     {:tag :property
                      :attrs {:name (str (first p))}
                      :content nil}))}))

(defmacro awhen [expr & body]
  `(let [~'it ~expr]
     (if ~'it
       (do ~@body))))

(import [java.io BufferedReader InputStreamReader]
        [java.net URL])

(defn joc-www []
  (-> "http://joyofclojure.com/hello"
      URL.
      .openStream
      InputStreamReader.
      BufferedReader.))

(defn test-joc []
  (let [stream (joc-www)]
    (with-open [page stream]
      (println (.readLine page)))))

(defmacro with-resource [bindings close-fn & body]
  (if (zero? (count bindings))
    `(do ~@body)
    `(let ~(subvec bindings 0 2)
       (try
         (with-resource ~(subvec bindings 2) ~close-fn ~@body)
         (finally (~close-fn ~(bindings 0)))))))

(declare collect-bodies build-contract)

(defmacro contract [name & forms]
  `(fn ~name ~@(collect-bodies forms)))

(defn collect-bodies [forms]
  (for [form (partition 3 forms)]
    (build-contract form)))

(defn build-contract [c]
  (let [args (first c)]
    `([~'f ~@args]
      ~(apply merge
             (for [con (rest c)]
               (condp = (first con)
                 'require (assoc {} :pre (vec (rest con)))
                 'ensure (assoc {} :post (vec (rest con)))
                 (throw (Exception. (str "Unknown tag " (first con)))))))
      (~'f ~@args))))
