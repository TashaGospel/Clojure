(ns testing
  (:require [clojure.string :as str])
  (:use [clojure.java.io :only (reader)])
  (:import (java.io File)))

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

