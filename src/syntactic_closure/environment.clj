(ns syntactic-closure.environment
  (:use [syntactic-closure.util :only [var->qualified-symbol]]))

;;
;; syntactic environment
;;
(defn make-environment [ns-name locals]
  {:ns-name ns-name, :locals locals})

(defn- toplevel-env [env]
  (ns-map (:ns-name env)))

(defn lookup [env n]
  (or ((:locals env) n)
      (if-let [ns-name (namespace n)]
        (let [var ((toplevel-env env) (symbol (name n)))]
          (if (and var (= (namespace (var->qualified-symbol var)) ns-name))
            var
            n))
        ((toplevel-env env) n))))

(defn add-to-environment [env id alias]
  (make-environment (:ns-name env)
                    (assoc (:locals env) id alias)))

(defn extend-environment [env ids]
  (make-environment (:ns-name env)
                    (:locals (reduce #(add-to-environment %1 %2 (gensym %2)) env ids))))

(defn filter-environment [names names-env else-env]
  (make-environment (:ns-name else-env)
                    (:locals (reduce (fn [env name]
                                       (if-let [var (lookup names-env name)]
                                         (add-to-environment env name var)
                                         env))
                                     else-env
                                     names))))
