(ns sim-test
  (:require [event-sourcing :as es]
            [joy.generators :refer (rand-map)]
            [clojure.set :as sql]))

(def PLAYERS #{{:player "Nick" :ability 8/25}
               {:player "Matt" :ability 13/50}
               {:player "Ryan" :ability 19/100}})

(defn lookup [db name]
  (first (sql/select
           #(= (:player %) name)
           db)))

(defn update-stats [db event]
  (let [player (lookup db (:player event))
        less-db (sql/difference db #{player})]
    (conj less-db
          (merge player (es/effect player event)))))

(defn commit-event [db event]
  (dosync (alter db update-stats event)))

(defn rand-event [{:keys [ability]}]
  (let [able (numerator ability)
        max (denominator ability)]
    (rand-map 1
              #(identity :result)
              #(if (< (rand-int max) able)
                 :hit
                 :miss))))

(defn rand-events [total player]
  (repeatedly total #(assoc (rand-event player)
                            :player (:player player))))

(def agent-for-player
  (memoize
    (fn [player-name]
      (doto (agent [])
        (set-error-handler! #(println "ERROR:" %1 %2))
        (set-error-mode! :fail)))))

(defn feed [db event]
  (let [a (agent-for-player (:player event))]
    (send a (fn [state]
              (commit-event db event)
              (conj state event)))))

(defn feed-all [db events]
  (doseq [event events]
    (feed db event))
  db)

(defn simulate [total players]
  (let [events (apply interleave
                      (for [player players]
                        (rand-events total player)))
        result (feed-all (ref players) events)]
    (apply await (map #(agent-for-player (:player %)) players))
    @result))
