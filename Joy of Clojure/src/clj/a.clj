(ns a
  "An implementation of the A* path-finding algorithm.")

(def world [[  1   1   1   1   1]
            [999 999 999 999   1]
            [  1   1   1   1   1]
            [  1 999 999 999 999]
            [  1   1   1   1   1]])

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
   (filter (fn [new-yx] (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %)) deltas))))

(defn estimate-cost [step-cost-est size y x]
  (* step-cost-est
     (- (+ size size) y x 2)))

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (or (:cost cheapest-nbr) 0)))

(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
     (estimate-cost step-cost-est size y x)))

(defn min-by [f coll]
  (if (seq coll)
    (reduce (fn [min-val cur]
              (if (< (f min-val) (f cur))
                min-val
                cur))
            coll)))

(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (repeat size (vec (repeat size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps]
        (let [[_ yx :as work-item] (first work-todo)
              work-todo-rest (disj work-todo work-item)
              nbrs (neighbors size yx)
              cheapest-nbr (min-by :cost (keep #(get-in routes %) nbrs))
              newcost (path-cost (get-in cell-costs yx) cheapest-nbr)
              oldcost (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps) routes work-todo-rest)
            (recur (inc steps)
                   (assoc-in routes yx
                             {:cost newcost
                              :yxs (conj (:yxs cheapest-nbr []) yx)})
                   (into work-todo-rest
                         (map (fn [w]
                                (let [[y x] w]
                                  [(total-cost newcost step-est size y x) w]))
                              nbrs)))))))))
