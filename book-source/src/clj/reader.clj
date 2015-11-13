(ns reader
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn lines []
  (with-open [rdr (io/reader "KeywordList.txt")]
    (doall (line-seq rdr))))

(defn count-tabs [s]
  (count (take-while #(= \tab %) s)))

(defn make-list []
  (loop [lines (lines) res []]
    (if (nil? lines)
      res
      (let [[cur & more] lines
            cur-level (count-tabs cur)]
        (recur (next lines)
               (conj res
                     (->> more
                          (take-while #(> (count-tabs %) cur-level))
                          (filter #(= (count-tabs %) (inc cur-level)))
                          (map str/trim)
                          vec
                          (list (str/trim cur)))))))))
