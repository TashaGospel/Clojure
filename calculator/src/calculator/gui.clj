(ns calculator.gui
  (:use calculator.core seesaw.core seesaw.mig)
  (:gen-class))

(native!)

(def layout [7 8 9 "/" "^" "⌫"
             4 5 6 "*" "(" ")"
             1 2 3 "-" "²" "√"
             0 "." "%" "+" "="])

(defn generate-mig-items [layout]
  (vec (concat
         [[(text :text "" :halign :right :class :text :font "Ubuntu Mono-BOLD-21") "span"]]
         (map (fn [c] [(button :text c :class :button :focusable? false)]) (pop layout))
         [[(button :text (peek layout) :class :button :focusable? false) "span"]])))

(defn add-char [s new-char]
  (cond
    (= "=" new-char) (try (calculate s) (catch Exception e "Error 404: You suck!"))
    (= "⌫" new-char) (apply str (butlast s))
    :else (str s new-char)))

(defn -main
  [& args]
  (let [f (frame :title "Calculator" :on-close :exit)
        grid (mig-panel
               :constraints ["wrap 6"
                             "[50, grow, fill]0[50, grow, fill]"
                             "[55, grow, fill]5[32, grow, fill]0[32,grow,fill]"]
               :items (generate-mig-items layout))
        t (first (select grid [:.text]))]
    (doseq [b (select grid [:.button])]
      (listen b :mouse-pressed (fn [e]
                                 (text! t (add-char (text t) (text b))))))
    (listen t :key-pressed (fn [e]
                             (if (= \newline (.getKeyChar e))
                               (text! t (add-char (text t) "=")))))
    (-> f
        (config! :content grid)
        pack!
        show!)))
