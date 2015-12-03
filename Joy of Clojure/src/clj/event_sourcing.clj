(ns event-sourcing)

(defn valid? [event]
  (boolean (:result event)))

(defn effect
  [{:keys [ab h] :or {ab 0 h 0}} event]
  (let [ab (inc ab)
        h (if (= :hit (:result event))
            (inc h)
            h)]
    {:ab ab :h h :avg (double (/ h ab))}))

(defn apply-effect [state event]
  (if (valid? event)
    (effect state event)))

(defn effect-all [state events]
  (reduce apply-effect state events))

(defn fx-timeline [state events]
  (reductions apply-effect state events))
