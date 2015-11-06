(ns pebble-game)

(declare human-turn computer-turn)

(defn announce-state [pebbles]
  (println "There are" pebbles "pebbles left."))

(defn get-human-turn [pebbles limit]
  (print "Number of pebbles to take: ") (flush)
  (loop [n (read)]
    (if (<= 1 n (min pebbles limit))
      n
      (do (print "Invalid turn. Retry: ") (flush)
          (recur (read)))))) 

(defn human-turn [pebbles limit]
  (announce-state pebbles)
  (if (zero? pebbles)
    (println "Computer wins!")
    #(computer-turn (- pebbles (get-human-turn pebbles limit)) limit)))

(defn computer-turn [pebbles limit]
  (announce-state pebbles)
  (if (zero? pebbles)
    (println "Human wins!")
    (let [n (rem pebbles (inc limit))
          turn (if (zero? n)
                 (inc (rand-int limit))
                 n)]
      (println "Computer takes" turn "pebbles.")
      #(human-turn (- pebbles turn) limit))))

(defn -main []
  (trampoline human-turn
    (do (print "Number of pebbles: ") (flush) (read))
    (do (print "Number of pebbles allowed to take per turn: ") (flush) (read))))
