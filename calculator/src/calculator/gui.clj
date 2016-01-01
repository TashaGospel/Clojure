(ns calculator.gui
  (use calculator.core seesaw.core)
  (:gen-class))

(native!)

(def layout [7 8 9 "/" "^" "⌫"
             4 5 6 "*" "(" ")"
             1 2 3 "-" "²" "√"
             0 "." "%" "+" "="])

(defn generate-mig-items [layout]
  (vec (concat
         [[(text :text "" :halign :right) "span"]]
         (map (fn [c] [(button :text c :id :char)]) (pop layout))
         [[(button :text (peek layout) :id :equal) "span"]])))

(defn -main
  [& args]
  (let [f (frame :title "Calculator")
        grid (mig-panel
               :constraints ["wrap 6"
                             "[50, grow, fill]0[50, grow, fill]"
                             "[55, grow, fill]5[32, grow, fill]0[32,grow,fill]"]
               :items (generate-mig-items layout))]
    (-> f
        (config! :content grid)
        pack!
        show!)))


; (config! a
;          :constraints["wrap 2"
;                       "[shrink 0]20px[200, grow, fill]"
;                       "[shrink 0]5px[]"]
;          :items [ ["name:"     ] [(text (or "me"))]
;                  ["category:" ] [(text (or "yep"))]
;                  ["date:"     ] [(text (or "1/2/3"))]
;                  ["comment:"  ] [(text (or "bad"))]])
