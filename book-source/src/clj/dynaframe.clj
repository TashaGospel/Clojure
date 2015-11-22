(ns dynaframe
  (:gen-class
    :name dynaframe
    :extends javax.swing.JFrame
    :implements [clojure.lang.IMeta]
    :prefix df-
    :state state
    :init init
    :constructors {[String] [String]
                   [] [String]}
    :methods [[display [java.awt.Container] void]
              ^:static [version [] String]])
  (:import (javax.swing JFrame JPanel JComponent)
           (java.awt Container BorderLayout)))

(defn df-init [title] [[title] (atom {::title title})])

(defn df-meta [this] @(.state this))

(defn version [] "1.0")

(defn df-display [this pane]
  (doto this
    (-> .getContentPane .removeAll)
    (.setContentPane (doto (JPanel.)
                       (.add pane BorderLayout/CENTER)))
    (.pack)
    (.setVisible true)))
