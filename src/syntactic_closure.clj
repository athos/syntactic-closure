(ns syntactic-closure
  (:refer-clojure :exclude [compile])
  (:use [syntactic-closure.environment :only [filter-environment]]))

(load "syntactic_closure/compile")
(load "syntactic_closure/specials")
(load "syntactic_closure/quasiquote")

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
  (syntactic-closur-ize f))

(defmacro sc-macro-transformer [f]
  (if-let [env (::env (meta &form))]
    `(make-syntactic-closure {} nil (~f '~env))
    `(let [env# (reduce (fn [env# [name# val#]]
                          (assoc env# name# name#))
                        {}
                        ~'&env)]
       (compile env# (make-syntactic-closure {} nil (~f env#))))))
