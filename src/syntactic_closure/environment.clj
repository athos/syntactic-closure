(ns syntactic-closure.environment)

;;
;; syntactic environment
;;
(defn- toplevel-env []
  (ns-map *ns*))

(defn lookup [env name]
  (or (env name)
      ((toplevel-env) name)))

(defn add-to-environment [env id alias]
  (assoc env id alias))

(defn extend-environment [env ids]
  (reduce (fn [env id] (add-to-environment env id (gensym id))) env ids))

(defn filter-environment [names names-env else-env]
  (reduce (fn [env [name val]]
            (if (names name)
              (assoc env name val)
              env))
          else-env
          names-env))
