(use 'clojure.string)
(defn ellipsize
  [words]
  (let [[a b c :as d] (split words #"\s+")]
    (join " " [a b c (if (> (count d) 3) "...")])))

