(def visitors (atom #{}))

(defn hello
  [username]
  (swap! visitors conj username)
  (str "Hello, " username))
