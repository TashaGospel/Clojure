(ns memoization)

(defprotocol CacheProtocol
  (lookup [cache e])
  (has? [cache e])
  (hit [cache e])
  (miss [cache e ret]))

(deftype BasicCache [cache]
  CacheProtocol
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [this item] this)
  (miss [_ item result]
    (BasicCache. (assoc cache item result))))

(defn through [cache f item]
  (if (has? cache item)
    (hit cache item)
    (miss cache item (delay (apply f item)))))

(deftype PluggableMemoization [f cache]
  CacheProtocol
  (lookup [_ item]
    (lookup cache item))
  (has? [_ item]
    (has? cache item))
  (hit [this item] this)
  (miss [this item result]
    (PluggableMemoization. f (miss cache item result))))

(defn memoization-impl [cache-impl]
  (let [cache (atom cache-impl)]
    (with-meta
      (fn [& args]
        (let [cs (swap! cache through (.f cache-impl) args)]
          @(lookup cs args)))
      {:cache cache})))

(defn slowly [x] (Thread/sleep 3000) x)

(def sometimes-slowly
  (memoization-impl (PluggableMemoization.
                      slowly
                      (BasicCache. {}))))
