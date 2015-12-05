(ns tetris.gui
  (:require [tetris.core :refer :all])
  (:import (java.awt Color Dimension BorderLayout)
           (javax.swing JPanel JFrame Timer JOptionPane JLabel)
           (java.awt.event KeyListener ActionListener))
  (:use tetris.import-static)
  (:gen-class))
(import-static java.awt.event.KeyEvent VK_DOWN VK_LEFT VK_RIGHT VK_Z VK_X)

(def dirs {VK_DOWN :hard-drop
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

(defn game-panel [frame board current-piece label score]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (draw-board g @board)
      (draw-piece g @current-piece)
      (.setText label (str @score)))
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

(defn piece-mover [piece board score frame]
  (reify
    ActionListener
    (actionPerformed [this e]
      (update-position piece :down board)
      (clear-board board score)
      (when (lose? @board)
        (JOptionPane/showMessageDialog frame (str "Game Over! Your score is " @score))
        (reset-game piece board score)))))

(defn game []
  (let [board (ref [])
        piece (ref (new-piece))
        score (ref 0)
        frame (JFrame. "Tetris")
        label (JLabel. "0")
        panel (game-panel frame board piece label score)
        timer (Timer. 75 panel)
        piece-move (piece-mover piece board score frame)
        piece-move-timer (Timer. 500 piece-move)]
    (doto label
      (.setHorizontalAlignment JLabel/RIGHT)
      (.setFont (.deriveFont (.getFont label) 50.0)))
    (doto panel
      (.add label BorderLayout/NORTH)
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))
    (.start timer)
    (.start piece-move-timer)))

(defn -main
  "Starts the game."
  [& args]
  (game))
