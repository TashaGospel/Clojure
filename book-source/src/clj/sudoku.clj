(ns sudoku
  "A solver for the 9x9 Sudoku game.
  Uses a brute-force approach."
  (:require [clojure.set :as set]))

(def b1 '[3 - - - - 5 - 1 -
          - 7 - - - 6 - 3 -
          1 - - - 9 - - - -
          7 - 8 - - - - 9 -
          9 - - 4 - 8 - - 2
          - 6 - - - - 5 - 1
          - - - - 4 - - - 6
          - 4 - 7 - - - 2 -
          - 2 - 6 - - - - 3])

(def b2 (vec (repeat 81 '-)))

(defn prep [board]
  "Prep for printing."
  ; The solve function produces all solutions.
  ; So we only take the first one.
  (map #(partition 3 %)
       (partition 9 (take 81 board))))

(defn print-board [board]
  (let [row-sep (apply str (repeat 25 "-"))]
    (println row-sep)
    (dotimes [i (count board)]
      (print "| ")
      (doseq [subrow (nth board i)]
        (doseq [cell (butlast subrow)]
          (print (str cell " ")))
        (print (last subrow) "| "))
      (println)
      (when (zero? (mod (inc i) 3))
        (println row-sep)))))

(defn rows [board size]
  (partition size board))

(defn row-for [board index size]
  (nth (rows board size) (quot index 9)))

(defn column-for [board index size]
  (let [i (mod index size)]
    (map #(nth % i)
         (rows board size))))

(defn subgrid-for [board i]
  (let [rows (rows board 9)
        sg-column (quot (mod i 9) 3)
        sg-row (quot (quot i 9) 3)
        grp-collumn (column-for (mapcat #(partition 3 %) rows) sg-column 3)
        grp (take 3 (drop (* sg-row 3) grp-collumn))]
    (flatten grp)))

(defn numbers-present-for [board i]
  (set
    (concat (row-for board i 9)
            (column-for board i 9)
            (subgrid-for board i))))

(defn possible-placements [board i]
  (set/difference #{1 2 3 4 5 6 7 8 9}
                  (numbers-present-for board i)))

(defn pos [pred coll]
  (for [[i v] (map-indexed vector coll) :when (pred v)] i))

; This function produces all the solutions
(defn solve [board]
  (if-let [[i] (and (some #{'-} board)
                    (pos #{'-} board))]
    (flatten (map #(solve (assoc board i %)) ; if a path fails it returns ()
                  (possible-placements board i))) ; so flatten to remove them
    board)) 
