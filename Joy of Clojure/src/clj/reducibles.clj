(ns reducibles
  (:require [clojure.core.reducers :as r]
            [clojure.core.protocols :as protos]))

(defn empty-range? [start end step]
  (if (pos? step)
    (>= start end)
    (<= start end)))

(defn reducible-range [start end step]
  (fn [f init]
    (loop [result init i start]
      (if (empty-range? i end step)
        result
        (recur (f result i) (+ i step))))))

(defn half [x] (/ x 2))

(defn half-transformer [reduce-f]
  (fn [result input]
    (reduce-f result (half input))))

(defn mapping [map-f]
  (fn [reduce-f]
    (fn [result input]
      (reduce-f result (map-f input)))))

(defn filtering [filter-f]
  (fn [reduce-f]
    (fn [result input]
      (if (filter-f input)
        (reduce-f result input)
        result))))

(defn mapcatting [reducible-builder]
  (fn [reduce-f]
    (fn [result input]
      (let [reducible (reducible-builder input)]
        (reducible reduce-f result)))))

(defn r-map [map-f reducible]
  (fn [reduce-f init]
    (reducible ((mapping map-f) reduce-f) init)))

(defn r-filter [filter-f reducible]
  (fn [reduce-f init]
    (reducible ((filtering filter-f) reduce-f) init)))

(def our-final-reducible
  (r-map #(/ % 2) (r-filter #(> % 4) (reducible-range 1 10 2))))

(defn core-r-map [map-f core-reducible]
  (r/reducer core-reducible (mapping map-f)))

(defn core-r-filter [filter-f core-reducible]
  (r/reducer core-reducible (filtering filter-f)))

(defn reduce-range [reduce-f init start end step]
  (loop [result init i start]
    (if (empty-range? i end step)
      result
      (recur (reduce-f result i) (+ i step)))))

(defn core-reducible-range [start end step]
  (reify protos/CollReduce
    (coll-reduce [this reduce-f init]
      (reduce-range reduce-f init start end step))
    (coll-reduce [this reduce-f]
      (if (empty-range? start end step)
        (reduce-f)
        (reducible-range start (+ start step) end step)))))

(defn core-f-map [map-f core-reducible]
  (r/folder core-reducible (mapping map-f)))
(defn core-f-filter [filter-f core-reducible]
  (r/folder core-reducible (filtering filter-f)))
