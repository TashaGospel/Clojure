(ns calculator.core)

(defn take-while-native
  "Same as take-while, but uses peek and pop instead of
  first and rest, so the evaluation is reversed for vectors."
  [pred coll]
  (lazy-seq (if (and (seq coll) (pred (peek coll)))
              (cons (peek coll) (take-while-native pred (pop coll))))))

(defn drop-while-native
  "Same as drop-while, but uses peek and pop instead of
  first and rest, so the evaluation is reversed for vectors."
  [pred coll]
  (lazy-seq (if (and (seq coll) (pred (peek coll)))
              (drop-while-native pred (pop coll))
              coll)))

(defn div [a b]
  (/ (float a) b))

(defn power [base expo]
  (Math/pow base expo))

(defn expo-10 [base expo]
  (* base (power 10 expo)))

(defn to-int [x]
  (- (int x) (int \0)))

(defn split-up
  "Splits a string into strings of decimals and non-decimals."
  [s]
  (re-seq #"\d+\.?\d*N?M?|\.\d+N?M?|\D" s))

(def operators {"+" + "-" - "*" * "/" div "^" power ; The pentalogy of basic operators
                "E" expo-10
                "%" [100 "/"]
                "²" [2 "^"]
                "√" [1/2 "^"]})

(def precedence {"+" 2 "-" 2 "*" 3 "/" 3 "^" 4
                 "%" 5 "²" 5 "√" 5 "E" 5})

(def associativity {"+" :left "-" :left "*" :left "/" :left "^" :right
                    "%" :left "²" :left "√" :right "E" :left})

(defn infix-to-postfix
  "Turns a collection (usually string) of infix notation to
  a vector of postfix notation.
  Uses Dijkstra's Shunting-yard algorithm.
  Numbers are turned into numbers,
  while operators remain strings."
  [s]
  (loop [[x & more] (split-up s) stack [] res []]
    (if x
        (cond
          (<= 0 (to-int (first x)) 9) (recur more stack (conj res (read-string x)))
          (= \. (first x)) (recur more stack (conj res (read-string (str "0" x))))
          (= "(" x) (recur more (conj stack x) res)
          (= ")" x) (recur more
                          (pop (vec (drop-while-native #(not= "(" %) stack)))
                          (apply conj res (take-while-native #(not= "(" %) stack)))
          (operators x)
          (let [op (if (= :right (associativity x)) < <=)]
            (recur more
                   (conj (vec (drop-while-native #(op (precedence x) (precedence % 0)) stack)) x)
                   (apply conj res (take-while-native #(op (precedence x) (precedence % 0)) stack)))))
      (apply conj res (reverse stack)))))

(defn calculate-from-postfix
  "Calculate an expression in postfix notation."
  [s]
  (loop [[x & more] s stack [0]]
    (if x
      (cond
        (number? x) (recur more (conj stack x))
        (operators x)
        (if (vector? (operators x))
          (recur (concat (operators x) more) stack)
          (recur more (conj (pop (pop stack)) ((operators x) (peek (pop stack)) (peek stack))))))
      (peek stack))))

(defn calculate [s]
  (calculate-from-postfix (infix-to-postfix s)))
