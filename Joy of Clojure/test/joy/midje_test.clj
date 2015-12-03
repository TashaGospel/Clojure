(ns joy.midje-test
  (:require [old-testing :as core])
  (:use midje.sweet))

(facts "About primes"
       (fact "Primes aren't divisible by 2"
             (drop 1 (core/primes)) => (map even?)))
