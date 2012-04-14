(ns syntactic-closure
  (:refer-clojure :exclude [compile])
  (:use [syntactic-closure.environment :only [make-environment filter-environment]]))

(load "syntactic_closure/compile")
(load "syntactic_closure/specials")
(load "syntactic_closure/quasiquote")

(defmacro define-syntax [name args & body]
  `(defmacro ~name ~args
     (let [~'&ns-name '~(ns-name *ns*)]
       ~@body)))

(defn- syntactic-closur-ize [f]
  (with-meta f {:type ::syntactic-closure}))

(defn syntactic-closure? [x]
  (if-let [m (meta x)]
    (= (:type m) ::syntactic-closure)))

(defn make-syntactic-closure [env free-vars exp]
  (let [free-vars' (set free-vars)]
    (syntactic-closur-ize
      (fn [free-names-env]
        (compile (filter-environment free-vars' free-names-env env) exp)))))

(defn capture-syntactic-environment [f]
  (syntactic-closur-ize
    (fn [env]
      (compile env (f env)))))

(defmacro sc-macro-transformer [f]
  `(if-let [env# (::env (meta ~'&form))]
     (make-syntactic-closure (make-environment ~'&ns-name {}) nil (~f env#))
     (let [env# (make-environment
                  (ns-name *ns*)
                  (into {} (map (fn [[name# val#]] [name# name#]) ~'&env)))]
       (compile env#
                (make-syntactic-closure (make-environment ~'&ns-name {})
                                        nil
                                        (~f env#))))))
