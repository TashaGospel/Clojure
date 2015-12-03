(ns udp
  (:refer-clojure :exclude [get]))

(defn beget [this proto]
  (assoc this ::prototype proto))

(defn get [m k]
  (if m
    (if-let [[_ v] (find m k)]
      v
      (recur (::prototype m) k))))

(def put assoc)

(def clone (partial beget {}))

(defmulti compiler :os)
(defmethod compiler ::unix [m] (get m :c-compiler))
(defmethod compiler ::osx [m] (get m :llvm-compiler))

(defmulti home :os)
(defmethod home ::unix [m] (get m :home))
(defmethod home ::bsd [m] "/home")
(derive ::osx ::unix)
(derive ::osx ::bsd)
(prefer-method home ::unix ::bsd)

(defmulti compile-cmd (juxt :os compiler))
(defmethod compile-cmd [::osx "clang"] [m]
  (str "/usr/bin/" (compiler m)))
(defmethod compile-cmd :default [m]
  (str "Don't no where to find " (get m :c-compiler)))
