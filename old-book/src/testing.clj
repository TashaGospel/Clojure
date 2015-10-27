(ns testing
  (:require [clojure.string :as str])
  (:use [clojure.java.io :only (reader)])
  (:import (java.io File FileInputStream InputStreamReader BufferedReader
                         FileOutputStream OutputStreamWriter BufferedWriter)
           (java.net URL Socket)))

(defn isSmall?
  "If number is less than 100, returns 'Yes', else 'No'"
  {:tag Boolean
   :added "1.7"}
  [number]
  (if (< number 100)
    (do (println "Saw a big number!") "Yes")
    (do (println "Saw a small number!") "No")))

(defn countDown
  "Returns a vector from x downto 1"
  [result x]
  (if (zero? x)
    result
    (recur (conj result x) (- x 1))))

(defn indexed [coll]
  (map-indexed vector coll))

(defn index-filter [pred coll]
  (when pred
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))

(defn non-blank? [line]
  (re-find #"\S" line))

(defn clojure-source?
  "Returns true if file is Clojure source code."
  [file]
  (.endsWith (.toString file) ".clj"))

(defn non-svn? [file]
  (not (.contains (.toString file) ".svn")))

(defn clojure-loc
  "Counts total the number of lines in every file using depth-first walk on base-file"
  {:tag Long}
  [base-file]
  (reduce
    +
      (for [file (file-seq base-file)
            :when (and (clojure-source? file) (non-svn? file))]
        (with-open [rdr (reader file)]
          (count (filter non-blank? (line-seq rdr)))))))

(defn bad-fibo
  "Returns the n-th fibonacci number. Uses simple stack-consuming recursion."
  {:tag Long}
  [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (bad-fibo (- n 1))
             (bad-fibo (- n 2)))))

(defn tail-fibo
  "Returns the n-th fibonacci number. Uses tail-call recursion."
  {:tag Long}
  [n]
  (letfn [(fib
            [cur next n]
            (if (zero? n)
              cur
              (recur next (+ cur next) (dec n))))]
    (fib 0N 1N n)))

(defn lazy-fibo
  "Returns a lazy (infinite!) sequence of fibonacci numbers. Uses lazy-seq."
  {:tag clojure.lang.LazySeq}
  ([] (concat [0N 1N] (lazy-fibo 0N 1N)))
  ([a b] (let [n (+ a b)]
           (lazy-seq
             (cons n (lazy-fibo b n))))))

(defn lib-fibo
  "Returns a lazy (infinite!) sequence of fibonacci numbers. Uses sequence library functions."
  {:tag clojure.lang.LazySeq}
  []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))

(defn count-head-pairs [coll]
  (loop [res 0 coll coll]
    (if (empty? coll)
      res
      (recur (if (= :h (first coll) (second coll))
               (inc res)
               res)
             (rest coll)))))

(def ^{:doc "Counts items matching a filter"}
  count-if
  (comp count filter))

(defn count-runs
  "Count run of length n where pred is true in coll."
  [n pred coll]
  (count-if #(every? pred %) (partition n 1 coll)))

(def ^Long better-count-head-pairs
  "Counts run of length 2 that are both heads."
  (partial count-runs 2 #(= :h %)))

(defrecord Message [sender text])
(def messages (ref ()))
(def backup-agent (agent "output/messages-backup.clj"))

(defn add-message [msg]
  (dosync (alter messages conj msg)))

(defn add-message-with-backup [msg]
  (dosync
    (let [snapshot (alter messages conj msg)]
      (send-off backup-agent (fn [filename]
                               (spit filename snapshot)
                               filename))
      snapshot)))

; (defn make-reader [src]
;   (-> (condp = (type src)
;         java.io.InputStream src
;         java.lang.String (FileInputStream. src)
;         java.io.File (FileInputStream. src)
;         java.net.Socket (.getInputStream src)
;         java.net.URL (if (= "file" (.getProtocol src))
;                        (-> src .getPath FileInputStream.)
;                        (.openStream src)))
;       InputStreamReader.
;       BufferedReader.))

; (defn make-writer [dst]
;   (-> (condp = (type dst)
;         java.io.OutputStream dst
;         java.lang.String (FileOutputStream. dst)
;         java.io.File (FileOutputStream. dst)
;         java.net.Socket (.getOutputStream dst)
;         java.net.URL (if (= "file" (.getProtocol dst))
;                        (-> dst .getPath FileOutputStream.)
;                        (throw (IllegalArgumentException.
;                                 "Can't write to non-file URL"))))
;       OutputStreamWriter.
;       BufferedWriter.))

(defprotocol IOFactory
  "A factory for things that write or read other things."
  (make-writer [this] "Creates a BufferedWriter.")
  (make-reader [this] "Creates a BufferedReader."))

; extend, extend-type, extend-protocol all serves the same purpose,
; and are functionally equivalent.
; extend-type and extend-protocol are macros of extend with a cleaner syntax.
; extend-type is useful for implementing new protocols for a type.
; extend-protocol is useful for implementing a protocol to several types.
(extend FileInputStream
  IOFactory
  {:make-writer
   (fn [dst]
     (throw (IllegalArgumentException. "Can't open as FileInputStream.")))
   :make-reader
   (fn [src]
     (-> src InputStreamReader. BufferedReader.))})

(extend FileOutputStream
  IOFactory
  {:make-writer
   (fn [dst]
     (-> dst OutputStreamWriter. BufferedWriter.))
   :make-reader
   (fn [src]
     (throw (IllegalArgumentException. "Can't open as FileOutputStream.")))})

(extend-type File
  IOFactory
  (make-writer [dst] (make-writer (FileOutputStream. dst)))
  (make-reader [src] (make-reader (FileInputStream. src))))

(extend-protocol IOFactory
  Socket
  (make-reader [src] (make-reader (.getInputStream src)))
  (make-writer [dst] (make-writer (.getOutputStream dst)))

  URL
  (make-reader [src] (make-reader (if (= "file" (.getProtocol src))
                                    (-> src .getPath FileInputStream.)
                                    (.openStream src))))
  (make-writer [dst] (make-writer (if ( = "file" (.getProtocol dst))
                                    (-> dst .getPath FileOutputStream.)
                                    (throw (IllegalArgumentException.
                                           "Can't read from non-file URL."))))))

(defn gulp [src]
  (let [sb (StringBuilder.)]
    (with-open [reader (make-reader src)]
      (loop [c (.read reader)]
        (if (neg? c)
          (str sb)
          (do
            (.append sb (char c))
            (recur (.read reader))))))))

(defn expectorate [dst content]
  (with-open [writer (make-writer dst)]
    (.write writer (str content))))

(defmacro chain
  ([x form] `(. ~x ~form))
  ([x form & more] `(chain (. ~x ~form) ~@more)))

(defmacro bench [form]
  `(let [start# (System/nanoTime)
         result# ~form]
     {:result result# :elapsed (- (System/nanoTime) start#)}))

(defmulti my-print class)

(defmethod my-print String [ob]
  (. *out* write ob))

(defmethod my-print nil [ob]
  (. *out* write "nil"))

(defmethod my-print Number [ob]
  (. *out* write (.toString ob)))

(defmethod my-print :default [ob]
  (.write *out* "#<")
  (.write *out* (. ob toString))
  (.write *out* ">"))

(defmethod my-print java.util.Collection [ob]
  (.write *out* "(")
  (.write *out* (str/join " " ob))
  (.write *out* ")"))

(defmethod my-print clojure.lang.IPersistentVector [ob]
  (.write *out* "[")
  (.write *out* (str/join " " ob))
  (.write *out* "]"))

(prefer-method my-print clojure.lang.IPersistentVector java.util.Collection)

(defn my-println [ob]
  (my-print ob)
  (.write *out* "\n"))

(defn class-available? [class-name]
  (try
    (. Class forName class-name) true
    (catch ClassNotFoundException _ false)))

(defn ^Long sum-to [^Long n]
  (loop [i 1
         res 0]
    (if (= i n)
      (+ res i)
      (recur (inc i) (+ res i)))))

(defn drill
  "Returns a function which can drill level levels into a coll"
  [f level]
  (if (= level 1)
    (partial apply f)
    (partial apply (drill f (dec level)))))
