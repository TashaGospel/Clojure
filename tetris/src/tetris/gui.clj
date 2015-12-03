(ns tetris.gui
  (:require [tetris.core :refer :all])
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event KeyListener ActionListener))
  (:use tetris.import-static)
  (:gen-class))
(import-static java.awt.event.KeyEvent VK_DOWN VK_LEFT VK_RIGHT VK_Z VK_X)

(def dirs {VK_DOWN :down
           VK_LEFT :left
           VK_RIGHT :right
           VK_Z :CW
           VK_X :CCW})

(defn fill-point [g {:keys [position color]}]
  (let [[x y width height] (point-to-screen-rect position)]
    (.setColor g color)
    (.fillRect g x y width height)))

(defn draw-board [g board]
  (doseq [point board]
    (fill-point g point)))

(defn draw-piece [g piece]
  (draw-board g (apply-to-board piece [])))

(defn game-panel [frame board current-piece]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (draw-board g @board)
      (draw-piece g @current-piece))
    (actionPerformed [e]
      (.repaint this))
    (keyPressed [e]
      (let [dir (get dirs (.getKeyCode e))]
        (when dir
          (update-position current-piece
                           dir
                           board))))
    (keyReleased [e])
    (keyTyped [e])
    (getPreferredSize []
      (Dimension. WINDOW_WIDTH WINDOW_HEIGHT))))

(defn piece-mover [piece board]
  (reify
    ActionListener
    (actionPerformed [this e]
      (update-position piece :down board))))

(defn game []
  (let [board (ref [])
        piece (ref (new-piece))
        frame (JFrame. "Tetris")
        panel (game-panel frame board piece)
        timer (Timer. 75 panel)
        piece-move (piece-mover piece board)
        piece-move-timer (Timer. 350 piece-move)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
    (.start timer)
    (.start piece-move-timer)))

(defn -main
  "Starts the game."
  [& args]
  (game))
