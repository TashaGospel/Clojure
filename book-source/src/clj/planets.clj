(ns planets
  (:require [clojure.core.logic :as log]))

(log/defrel orbits orbital body)

(log/fact orbits :mercury :sun)
(log/fact orbits :venus :sun)
(log/fact orbits :earth :sun)
(log/fact orbits :mars :sun)
(log/fact orbits :jupiter :sun)
(log/fact orbits :saturn :sun)
(log/fact orbits :uranus :sun)
(log/fact orbits :neptune :sun)

(log/defrel stars star)

(log/fact stars :sun)

(defn planeto [body]
  (log/fresh [star]
             (stars star)
             (orbits body star)))

(log/fact stars :alpha-centauri)
(log/fact orbits :Bb :alpha-centauri)

(defn satelliteo [body]
  (log/fresh [planet]
             (orbits body planet)
             (planeto planet)))

(log/fact orbits :moon :earth)
