(ns tetris.core
  (:require [clojure.set :as set]))

(def WINDOW_HEIGHT 640)
(def WINDOW_WIDTH 400)
(def BOARD_WIDTH 10)
(def BOARD_HEIGHT 16)
(def PIECE_HEIGHT (/ WINDOW_HEIGHT BOARD_HEIGHT))
(def PIECE_WIDTH (/ WINDOW_WIDTH BOARD_WIDTH))

; Coordinates are represented like this:
; | - - - - >
; |         y
; |
; v x
;
; But Swing is reversed:
; | - - - - >
; |         x
; |
; v y
;
; The board is represented as a vector of maps,
; each map has the form
; {:position [x y] :color (Color. a b c)}
;
; Pieces are represented in the form
; {:position [] :color (Color.) :id 1 :rotation-id 0}
; 

(def pieces
  [{:color (java.awt.Color. 0   255 255) :id 0 :rotation-id 0}   ; I
   {:color (java.awt.Color. 0   0   255) :id 1 :rotation-id 0}   ; J
   {:color (java.awt.Color. 255 165 0  ) :id 2 :rotation-id 0}   ; L
   {:color (java.awt.Color. 255 255 0  ) :id 3 :rotation-id 0}   ; O
   {:color (java.awt.Color. 0   255 0  ) :id 4 :rotation-id 0}   ; S
   {:color (java.awt.Color. 128 0   128) :id 5 :rotation-id 0}   ; T
   {:color (java.awt.Color. 255 0   0  ) :id 6 :rotation-id 0}]) ; Z

(def rotation
  [[[[1 0] [1 1] [1 2] [1 3]] ; I
    [[0 2] [1 2] [2 2] [3 2]]
    [[2 0] [2 1] [2 2] [2 3]]
    [[0 1] [0 2] [0 3] [0 4]]]
   [[[0 0] [1 0] [1 1] [1 2]] ; J
    [[0 1] [0 2] [1 1] [2 1]]
    [[1 0] [1 1] [1 2] [2 2]]
    [[0 1] [1 1] [2 1] [2 0]]]
   [[[0 2] [1 0] [1 1] [1 2]] ; L
    [[0 1] [1 1] [2 1] [2 2]]
    [[1 0] [1 1] [1 2] [2 0]]
    [[0 0] [0 1] [1 1] [2 1]]]
   [[[0 1] [0 2] [1 1] [1 2]] ; O
    [[0 1] [0 2] [1 1] [1 2]]
    [[0 1] [0 2] [1 1] [1 2]]
    [[0 1] [0 2] [1 1] [1 2]]]
   [[[0 1] [0 2] [1 0] [1 1]] ; S
    [[0 1] [1 1] [1 2] [2 2]]
    [[1 1] [1 2] [2 0] [2 1]]
    [[0 0] [1 0] [1 1] [2 1]]]
   [[[0 1] [1 0] [1 1] [1 2]] ; T
    [[0 1] [1 1] [1 2] [2 1]]
    [[1 0] [1 1] [1 2] [2 1]]
    [[0 1] [1 0] [1 1] [2 1]]]
   [[[0 0] [0 1] [1 1] [1 2]] ; Z
    [[0 2] [1 1] [1 2] [2 1]]
    [[1 0] [1 1] [2 1] [2 2]]
    [[0 1] [1 0] [1 1] [2 0]]]])

(defn abs
  "Returns the absolute value of a number."
  [x]
  (if (neg? x) (- x) x))

(defn bound
  "Returns the right, left, upper, or lower bound of the piece."
  [piece direction]
  (case direction
    :upper (apply min (map first piece))
    :lower (apply max (map first piece))
    :left (apply min (map second piece))
    :right (apply max (map second piece))))

(defn overlaps
  "Return a seq of the positions where the
  piece overlaps with any other objects on the board."
  [piece board]
  (set/intersection
    (set (map :position board))
    (set piece)))

(defn out-of-bounds
  "Returns a seq of the positions where the piece
  is out of bounds of the board."
  [piece size-x size-y]
  (remove (fn [[x y]]
            (and (<= x (dec size-x))
                 (<= 0 y (dec size-y))))
          piece))

(defn illegal-positions
  "Return a seq of the illegal positions of a piece
  on the board."
  [piece board]
  (seq (concat (out-of-bounds piece BOARD_HEIGHT BOARD_WIDTH)
               (overlaps piece board))))

(defn point-to-screen-rect
  "Converts a point on the board to a rectangle on the screen."
  [[x y]]
  [(* y PIECE_WIDTH) (* x PIECE_HEIGHT) PIECE_WIDTH PIECE_HEIGHT])

(defn rotate
  "Rotates a piece clockwise :CW or counter-clockwise :CCW."
  [{:keys [position color id rotation-id]} direction board]
  (let [f (case direction :CW inc :CCW dec)
        [pivot-x pivot-y] (map -
                               (first position)
                               (first (get-in rotation [id rotation-id])))]
    (map (fn [[x y]] [(+ x pivot-x) (+ y pivot-y)])
         (get-in rotation [id (mod (f rotation-id) 4)]))))

(defn move
  "Moves a piece :left or :right or :down,
  or rotate it :CW or :CCW.
  Returns a map."
  [{:keys [position rotation-id] :as piece} direction board]
  (let [moved (case direction
                :left (map (fn [[x y]] [x (dec y)]) position)
                :right (map (fn [[x y]] [x (inc y)]) position)
                :down (map (fn [[x y]] [(inc x) y]) position)
                (rotate piece direction board))
        f (case direction :CW inc :CCW dec identity)]
    (if (illegal-positions moved board)
      piece
      (assoc piece :position moved :rotation-id (mod (f rotation-id) 4)))))

(defn apply-to-board
  "Apply a piece to the board."
  [{:keys [position color]} board]
  (reduce #(conj % {:position %2 :color color}) board position))

(defn new-piece
  "Returns a random piece."
  []
  (let [piece (rand-nth pieces)]
    (assoc piece :position
           (map (fn [[x y]] [(- x 2) (+ y (quot BOARD_WIDTH 2) -2)])
                (get-in rotation [(:id piece) 0])))))

(defn update-position [piece direction board]
  (let [moved (move @piece direction @board)]
    (if (and (= :down direction) (= (:position moved) (:position @piece)))
      (dosync (alter board
                     #(apply-to-board @piece %1))
              (ref-set piece (new-piece)))
      (dosync
        (ref-set piece moved)))))

(defn reset-game [piece board]
  (dosync
    (ref-set board [])
    (ref-set piece (new-piece))))
