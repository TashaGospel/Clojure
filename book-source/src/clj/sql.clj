(ns sql
  (:require [clojure.string :as str]))

(defn shuffle-expr [expr]
  (if (coll? expr)
    (if (= (first expr) `unquote)
      "?"
      (let [[op & args] expr]
        (str "("
             (str/join (str " " op " ")
                       (map shuffle-expr args))
             ")")))
    expr))

(defn process-where-clause [processor expr]
  (str " WHERE " (processor expr)))

(defn process-left-join-clause [processor table _ expr]
  (str " LEFT JOIN " table
       " ON " (processor expr)))

(defn process-from-clause [processor table & joins]
  (apply str " FROM " table (map processor joins)))

(defn process-select-clause [processor fields & clauses]
  (apply str "SELECT " (str/join ", " fields)
         (map processor clauses)))

(declare apply-syntax)

(def ^:dynamic *clause-map*
  {'SELECT (partial process-select-clause apply-syntax)
   'FROM (partial process-from-clause apply-syntax)
   'LEFT-JOIN (partial process-left-join-clause shuffle-expr)
   'WHERE (partial process-where-clause shuffle-expr)})

(defn apply-syntax [[op & args]]
  (apply (get *clause-map* op) args))

(defmacro SELECT [& args]
  {:query (apply-syntax (cons 'SELECT args))
   :bindings (vec (for [n (filter coll? (tree-seq coll? seq args))
                        :when (= (first n) `unquote)]
                    (second n)))})

(defn example-query [max]
  (SELECT [a b c]
          (FROM X
                (LEFT-JOIN Y :ON (= X.a Y.b)))
          (WHERE (AND (< a 5) (< b ~max)))))
