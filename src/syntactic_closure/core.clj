(ns syntactic-closure.core
  (:refer-clojure :exclude [compile])
  (:require [syntactic-closure.environment :as env]
            [syntactic-closure.util :as util]))

(load "quasiquote")

;;
;; public interfaces
;;
(declare compile)

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
        (compile (env/filter-environment free-vars' free-names-env env) exp)))))

(defn capture-syntactic-environment [f]
  (syntactic-closur-ize
    (fn [env]
      (compile env (f env)))))

(defmacro sc-macro-transformer [f]
  `(if-let [env# (::env (meta ~'&form))]
     (make-syntactic-closure (env/make-environment ~'&ns-name {}) nil (~f env#))
     (let [env# (env/make-environment
                  (ns-name *ns*)
                  (into {} (map (fn [[name# val#]] [name# name#]) ~'&env)))]
       (compile env#
                (make-syntactic-closure (env/make-environment ~'&ns-name {})
                                        nil
                                        (~f env#))))))

;;
;; compiler
;;
(declare compile-exprs compile-special special?)

(defn- compile-symbol [env sym]
  (let [var (env/lookup env sym)]
    (cond (nil? var) sym
          (var? var) (util/var->qualified-symbol var)
          (class? var) (symbol (.getName var))
          :else var)))

(defn- compile-seq [env exp]
  (let [op (first exp), m (env/lookup env op)]
    (cond (and (var? m) (util/macro? m))
          (compile env (apply m (util/add-meta exp {::env env}) env (rest exp)))

          (special? op)
          (compile-special env exp)

          :else (compile-exprs env exp))))

(defn- compile-syntactic-closure [env sc]
  (sc env))

(defn compile [env exp]
  (cond (syntactic-closure? exp) (compile-syntactic-closure env exp)
        (symbol? exp) (compile-symbol env exp)
        (and (list? exp) (empty? exp)) '()
        (seq? exp) (compile-seq env exp)
        (vector? exp) (vec (compile-exprs env exp))
        (map? exp) (reduce (fn [map [key val]]
                             (assoc map (compile env key) (compile env val)))
                           {}
                           exp)
        (set? exp) (compile-exprs env exp)
        :else exp))

(defn- compile-exprs [env exprs]
  (map #(compile env %) exprs))

;;
;; compilers for special forms
;;
(def ^:private %specials
  '#{def if let* fn* quote do loop* recur var})

(defn- special? [op]
  (%specials op))

(defmulti ^:private compile-special (fn [env [op & _]] op))

;;; NB:
;;;  I don't know how (qq (def foo bar)) or (qq (def ~(make-syntactic-closure env nil 'foo) bar))
;;;  should behave.
;;;  For the time being, the first argument of `def' is never renamed.
(defmethod compile-special 'def [env exp]
  (let [[_ var init] exp]
    (let [var' (compile env var)
          var' (if (namespace var') (symbol (name var')) var')
          env' (env/add-to-environment env var' var')]
     `(def ~var' ~(compile env' init)))))

(defmethod compile-special 'if [env exp]
  (let [[_ test then else] exp]
    `(if ~(compile env test)
         ~(compile env then)
         ~(compile env else))))

(defmethod compile-special 'let* [env exp]
  (let [[_ bindings & body] exp
        bindings' (partition 2 bindings)
        names (map first bindings')
        inits (map second bindings')
        env' (env/extend-environment env names)]
    `(let* ~(vec (interleave (compile-exprs env' names)
                             (compile-exprs env' inits)))
           ~@(compile-exprs env' body))))

;; FIXME:
;;  Forms like (fn* foo [arg ...] body ...) are not supported.
(defmethod compile-special 'fn* [env exp]
  (let [[_ args & body] exp
        env' (env/extend-environment env args)]
    `(fn* ~(compile env' args)
          ~@(compile-exprs env' body))))

(defmethod compile-special 'quote [env exp]
  exp)

(defmethod compile-special 'do [env exp]
  (let [[_ & exprs] exp]
    `(do ~@(compile-exprs env exprs))))

(defmethod compile-special 'loop* [env exp]
  (let [[_ bindings & body] exp
        bindings' (partition 2 bindings)
        names (map first bindings')
        inits (map second bindings')
        env' (env/extend-environment env names)]
    `(loop* ~(vec (interleave (compile-exprs env' names)
                              (compile-exprs env' inits)))
            ~@(compile-exprs env' body))))

(defmethod compile-special 'recur [env exp]
  (let [[_ & args] exp]
    `(recur ~@(compile-exprs env args))))

(defmethod compile-special 'var [env exp]
  exp)

(defmethod compile-special 'set! [env exp]
  (let [[_ var val] exp]
    `(set! ~(compile env var)
           ~(compile env val))))
