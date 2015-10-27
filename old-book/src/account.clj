(ns account)

(defstruct account :id :tag :balance)

(defmulti interest-rate :tag)
(defmethod interest-rate ::Savings [_] 0M)
(defmethod interest-rate ::Checking [_] 0.05M)

(defmulti account-level :tag)
(defmethod account-level ::Checking [acct]
  (if (>= (:balance acct) 5000) ::Premium ::Basic))
(defmethod account-level ::Savings [acct]
  (if (>= (:balance acct) 1000) ::Premium ::Basic))

(derive ::Savings ::Account)
(derive ::Checking ::Account)

(defmulti service-charge (fn [acct] [(account-level acct) (:tag acct)]))
(defmethod service-charge [::Premium ::Account] [_] 0)
(defmethod service-charge [::Basic ::Checking] [_] 25)
(defmethod service-charge [::Basic ::Savings] [_] 10)

