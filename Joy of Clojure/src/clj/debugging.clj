(ns debugging)

(defn readr [prompt exit-code]
  (let [input (clojure.main/repl-read prompt exit-code)]
    (if (= input ::tl)
      exit-code
      input)))

(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [s] `'~s) symbols)
            symbols)))

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(apply concat ctx)]
      ~expr)))

(defmacro break []
  `(clojure.main/repl
     :prompt #(print "debug=> ")
     :read readr
     :eval (partial contextual-eval (local-context))))
