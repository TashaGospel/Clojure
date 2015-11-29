(ns unify
  (:require [clojure.walk :as walk]))

(defn lvar?
  "Determines if a symbol is a logic variable."
  [x]
  (boolean
    (when (symbol? x)
      (re-matches #"^\?.*" (name x)))))

(defn satisfy [l r knowledge]
  (let [L (get knowledge l l)
        R (get knowledge r r)]
    (cond
      (= L R) knowledge
      (lvar? L) (assoc knowledge L R)
      (lvar? R) (assoc knowledge R L)
      (every? seq? [L R]) (satisfy (rest L) (rest R)
                                   (satisfy (first L) (first R) knowledge))
      :else nil)))

(defn subst [term binds]
  (walk/prewalk #(if (lvar? %)
                   (get binds % %)
                   %)
                term))

(defn meld [term1 term2]
  (->> {}
       (satisfy term1 term2)
       (subst term1)))
