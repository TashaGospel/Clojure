(ns seesaw-tutorial)

(native!)

(def f (frame :title "View source and documentation of functions" :minimum-size [640 :by 400]))

(defn display [content]
  (config! f :content content)
  content)

(def lb (listbox :model (-> 'seesaw.core ns-publics keys sort)))

(def area (text :multi-line? true :font "MONOSPACED-PLAIN-14"))

(def split (left-right-split (scrollable lb) (scrollable area) :divider-location 1/3))

(defn doc-str [s] (-> (symbol "seesaw.core" (str s)) resolve meta :doc))

(defn source-str [s] (-> (symbol "seesaw.core" (str s)) clojure.repl/source-fn))

(def rbs (for [i [:source :doc]]
           (radio :id i :class :type :text (name i))))

(def group (button-group))

(config! rbs :group group)

(listen lb :selection
        (fn [e]
          (when-let [s (selection e)]
            (when-let [mode (selection group)]
              (-> area
                  (text! (if (= (id-of mode) :doc)
                           (doc-str s)
                           (source-str s)))
                  (scroll! :to :top))))))

(show! f)
(display (border-panel
           :north (horizontal-panel :items rbs)
           :center split
           :vgap 5 :hgap 5 :border 5))
