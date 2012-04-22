(ns syntactic-closure.environment
  (:use [clojure.string :only [replace]]
        [syntactic-closure.util :only [var->qualified-symbol]]))

;;
;; syntactic environment
;;
(defn make-environment [ns-name locals]
  {:ns-name ns-name, :locals locals})

(defn- toplevel-env [env]
  (ns-map (:ns-name env)))

(defn lookup [env n]
  (or ((:locals env) n)
      (let [s (str n)]
        (cond (and (not (namespace n)) (= (first s) \.))
              n

              (and (not (namespace n)) (= (last s) \.))
              (let [csym (symbol (replace s #".$" ""))
                    class (ns-resolve (:ns-name env) csym)]
                (if (class? class)
                  (symbol (str (.getName class) "."))
                  n))

              :else
              (or (if (namespace n)
                    (let [maybe-class ((toplevel-env env) (symbol (namespace n)))]
                      (and (class? maybe-class)
                           (symbol (.getName maybe-class) (name n)))))
                  (ns-resolve (:ns-name env) n))))))

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
