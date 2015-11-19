(ns testproject.core-test
  (:require [testproject.core :as core])
  (:use midje.sweet))

(facts "About times2"
       (fact "Times2 doubles a number"
             (core/times2 5) => 10
             (core/times2 0) => zero?)
       (fact "Times2 works any numerical datatype"
             (core/times2 2.3) => 4.6
             (core/times2 3/5) => 6/5)
       (tabular "Times2 is just like adding a number twice"
                (fact (+ ?x ?x) => (core/times2 ?x))
                ?x
                1 2 3 4 -1))
