(ns tetris.core-test
  (:require [midje.sweet :refer :all]
            [tetris.core :refer :all]))

(facts "About abs"
       (fact
         (abs -1) => 1
         (abs 0) => 0
         (count (filter neg? (map abs
                                  (range Short/MIN_VALUE Short/MAX_VALUE)))) => 0))

(facts "About bound"
       (fact "bound returns the bound of a piece"
             (bound [[0 0] [0 1] [1 1] [1 2]] :left) => 0
             (tabular
               (fact (bound [[0 1] [1 0] [1 1] [1 2]] ?x) => ?y)
               ?x     ?y
               :left  0
               :right 2
               :upper 0
               :lower 1)))

(facts "About illegal-positions"
       (fact
         (illegal-positions [[0 0] [0 1] [1 1] [1 0]]
                            [{:position [2 2]} {:position [3 3]}])
         => nil
         (illegal-positions [[0 0] [0 1] [BOARD_HEIGHT 1] [3 3]]
                            [{:position [2 2]} {:position [3 3]}])
         => [[BOARD_HEIGHT 1] [3 3]]))

(facts "About apply-to-board"
       (fact
         (let [color (java.awt.Color. 0 255 0)]
           (apply-to-board {:position [[0 0] [0 1] [1 0] [1 1]] :color color} [])
           => [{:position [0 0] :color color}
               {:position [0 1] :color color}
               {:position [1 0] :color color}
               {:position [1 1] :color color}])))
