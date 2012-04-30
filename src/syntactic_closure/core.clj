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
  (let [op (first exp)]
    (if (symbol? op)
      (let [m (env/lookup env op)]
        (cond (and (var? m) (util/macro? m))
              (compile env (apply m (util/add-meta exp {::env env}) env (rest exp)))

              (special? op)
              (compile-special env exp)

              :else (compile-exprs env exp)))
      (compile-exprs env exp))))

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
  '#{def if let* fn* quote do loop* recur var set! new . letfn* clojure.core/import*
     try throw monitor-enter monitor-exit deftype* case* reify*})

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
                             (compile-exprs env inits)))
           ~@(compile-exprs env' body))))

(defmethod compile-special 'fn* [env exp]
  (let [[_ maybe-name & body] exp
        name (if (symbol? maybe-name) maybe-name nil)
        maybe-args (if name (first body) maybe-name)]
    (if (vector? maybe-args)
      (let [args maybe-args
            body (if name (rest body) body)
            env' (env/extend-environment env (if name (cons name args) args))]
        `(fn* ~@(and name [(compile env' name)])
              ~(compile env' args)
              ~@(compile-exprs env' body)))
      (let [body (if name body (cons maybe-name body))
            env' (if name (env/extend-environment env [name]) env)]
       `(fn* ~@(and name [(compile env' name)])
             ~@(for [[args & body] body
                     :let [env'' (env/extend-environment env' args)]]
                 `(~(compile env'' args)
                   ~@(compile-exprs env'' body))))))))

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
                              (compile-exprs env inits)))
            ~@(compile-exprs env' body))))

(defmethod compile-special 'recur [env exp]
  (let [[_ & args] exp]
    `(recur ~@(compile-exprs env args))))

(defmethod compile-special 'var [env exp]
  (let [[_ exp'] exp]
    `(var ~(compile env exp'))))

(defmethod compile-special 'set! [env exp]
  (let [[_ var val] exp]
    `(set! ~(compile env var)
           ~(compile env val))))

(defmethod compile-special 'new [env exp]
  (let [[_ class & args] exp]
    `(new ~(compile env class)
          ~@(compile-exprs env args))))

(defmethod compile-special '. [env exp]
  (let [[_ x method-or-field & args] exp]
    `(. ~(compile env x)
        ~method-or-field
        ~@(compile-exprs env args))))

(defmethod compile-special 'letfn* [env exp]
  (let [[_ fns & body] exp
        fns' (partition 2 fns)
        fnames (map first fns')
        fexprs (map second fns')
        env' (env/extend-environment env fnames)]
    `(letfn* ~(vec (mapcat (fn [[fname fexpr]]
                             [(compile env' fname)
                              (compile env' fexpr)])
                           (map vector fnames fexprs)))
             ~@(compile-exprs env' body))))

(defmethod compile-special 'clojure.core/import* [env exp]
  exp)

(defmethod compile-special 'try [env exp]
  (let [[_ & body] exp
        [exprs rest] (split-with #(not (and (seq? %) (= (first %) 'catch))) body)
        [catch-clauses finally-clause]
        (split-with #(not (and (seq? %) (= (first %) 'finally))) rest)]
    `(try ~@(compile-exprs env exprs)
          ~@(for [[_ class ename & body] catch-clauses
                  :let [env' (env/extend-environment env [ename])]]
              `(catch ~(compile env class) ~(compile env' ename)
                 ~@(compile-exprs env' body)))
          ~@(if (empty? finally-clause)
              nil
              (let [[_ & body] (first finally-clause)]
                `((finally ~@(compile-exprs env body))))))))

(defmethod compile-special 'throw [env exp]
  (let [[_ exp'] exp]
    `(throw ~(compile env exp'))))

(defmethod compile-special 'monitor-enter [env exp]
  (let [[_ exp'] exp]
    `(monitor-enter ~(compile env exp'))))

(defmethod compile-special 'monitor-exit [env exp]
  (let [[_ exp'] exp]
    `(monitor-exit ~(compile env exp'))))

(defmethod compile-special 'deftype* [env exp]
  (let [[_ tag class fields implements interfaces & methods] exp
        env' (env/make-environment (:ns-name env) {})]
    `(deftype* ~tag ~class ~fields :implements ~interfaces
       ~@(for [[name args & body] methods
               :let [env'' (env/extend-environment env' args)]]
           `(~name ~(vec (compile-exprs env'' args))
                   ~@(compile-exprs env'' body))))))

(defmethod compile-special 'case* [env exp]
  (let [[_ x & rest] exp]
    `(case* ~(compile env x)
            ~@rest)))

(defmethod compile-special 'reify* [env exp]
  (let [[_ interfaces & methods] exp]
    `(reify*
       ~(vec (compile-exprs env interfaces))
       ~@(for [[name args & body] methods
               :let [env' (env/extend-environment env args)]]
           `(~name ~(vec (compile-exprs env' args))
                   ~@(compile-exprs env' body))))))
