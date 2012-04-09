(ns syntactic-closure.util)

(defn error [& msgs]
  (throw (Exception. (apply str msgs))))

(defn var->qualified-symbol [^clojure.lang.Var var]
  (let [^clojure.lang.Namespace ns (.ns var)]
    (symbol (str (.name ns)) (str (.sym var)))))

(defn macro? [var]
  (if-let [m (meta var)]
    (:macro m)))

(defn add-meta [x m]
  (if (meta x)
    (vary-meta x #(into % m))
    (with-meta x m)))
