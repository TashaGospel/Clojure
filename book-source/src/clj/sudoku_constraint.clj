(ns sudoku-constraint
  "A solver for the 9x9 Sudoku game.
  Uses contraints and core.logic."
  (:require [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]
            [sudoku :refer (print-board prep) :as sudoku]))

(defn rowify [board]
  (->> board
       (partition 9)
       (map vec)
       vec))

(defn colify [rows]
  (apply map vector rows))

(defn subgrid [rows]
  (partition 9
             (for [row [0 3 6]
                   col [0 3 6]
                   x (range row (+ 3 row))
                   y (range col (+ 3 col))]
               (get-in rows [x y]))))

(defn logic-board []
  (repeatedly 81 logic/lvar))

(defn init [[lv & lvs] [cell & cells]]
  (if lv
    (logic/fresh []
                 (if (= cell '-)
                   logic/succeed
                   (logic/== lv cell))
                 (init lvs cells))
    logic/succeed))

(defn solve-logically [board]
  (let [legal-nums (fd/interval 1 9)
        lvars (logic-board)
        rows (rowify lvars)
        cols (colify rows)
        grids (subgrid rows)]
    (logic/run 1 [q]
               (init lvars board)
               (logic/everyg #(fd/in % legal-nums) lvars)
               (logic/everyg fd/distinct rows)
               (logic/everyg fd/distinct cols)
               (logic/everyg fd/distinct grids)
               (logic/== lvars q))))
