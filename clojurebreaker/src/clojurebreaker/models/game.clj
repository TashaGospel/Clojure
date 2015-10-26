(ns clojurebreaker.models.game
  (:require [clojure.math.combinatorics :as comb]
            [clojure.data :as data]))

(defn create []
  (vec (repeatedly 4 #(rand-nth ["r" "g" "b" "y"]))))

(defn exact-matches
  "Returns the number of positions in two collections where the vals are equal."
  [c1 c2]
  (count (remove nil? (last (data/diff c1 c2)))))

(defn unordered-matches
  "Given two collections, return a map where each key is an item
  in both collections, and each value is the number of times the
  value occurs in the collection with fewest occurrences."
  [c1 c2]
  (merge-with min
              (select-keys (frequencies c1) c2)
              (select-keys (frequencies c2) c1)))

(defn score
  [c1 c2]
  (let [exact (exact-matches c1 c2)
        unordered (apply + (vals (unordered-matches c1 c2)))]
    {:exact exact :unordered (- unordered exact)}))
