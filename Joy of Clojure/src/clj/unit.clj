(ns unit)

(defn convert [context descriptor]
  (reduce (fn [result [amount unit]]
            (let [v (get context unit)]
              (+ result
                 (if (vector? v)
                   (* amount (convert context v))
                   (* amount v)))))
          0
          (partition 2 descriptor)))

(def distance-reader
  (partial convert {:m 1 :km 1000 :cm 1/100 :mm [1/10 :cm]}))

(def time-reader
  (partial convert {:sec 1 :min 60 :hr [60 :min] :day [24 :hr]}))

(defn relative-units [context unit]
  (if-let [spec (get context unit)]
    (if (vector? spec)
      (convert context spec)
      spec)
    (throw (IllegalArgumentException. (str "Undefined unit: " unit)))))

(defmacro defunits-of [name base-unit & conversions]
  (let [units-map (into {base-unit 1}
                        (map vec (partition 2 conversions)))]
    `(defmacro ~(symbol (str "unit-of-" name))
       [magnitude# unit#]
       `(* ~magnitude#
           ~(case unit#
              ~@(mapcat (fn [[u# & _#]]
                          `[~u# ~(relative-units units-map u#)])
                        units-map))))))
