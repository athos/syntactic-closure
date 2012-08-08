(ns syntactic-closure.core
  (:refer-clojure :exclude [compile])
  (:require [syntactic-closure.environment :as env]
            [syntactic-closure.util :as util]))

(load "quasiquote")

(def ^:dynamic *env* nil)

(defn toplevel-env []
  (env/make-environment (ns-name *ns*) {}))

(defmacro current-env []
  `(env/make-environment (ns-name *ns*)
                         (zipmap (keys ~'&env) (keys ~'&env))))

;;
;; public interfaces
;;
(declare compile)

(defmacro define-syntax [name & body]
  `(defmacro ~name [& args#]
     (let [transformer# (do ~@body)]
       (if *env*
         (transformer# ~'&form (toplevel-env))
         (binding [*env* (current-env)]
           (compile (transformer# ~'&form (toplevel-env))))))))

(defmacro let-syntax [bindings & body]
  (let [env (or *env* (current-env))
        bindings' (for [[fname fexpr] (partition 2 bindings)
                        :let [transformer (binding [*env* (toplevel-env)]
                                            (eval fexpr))]]
                    [fname (fn [form] (transformer form env))])]
    (binding [*env* (env/extend-environment env
                                            (map first bindings')
                                            (map second  bindings'))]
      (compile `(do ~@body)))))

(defmacro letrec-syntax [bindings & body]
  (let [env (atom (or *env* (current-env)))
        bindings' (for [[fname fexpr] (partition 2 bindings)
                        :let [transformer (binding [*env* (toplevel-env)]
                                            (eval fexpr))]]
                    [fname (fn [form] (transformer form @env))])]
    (doseq [[fname transformer] bindings']
      (swap! env #(env/add-to-environment % fname transformer)))
    (binding [*env* env]
      (compile `(do ~@body)))))

(defn syntactic-closure? [x]
  (= (type x) ::syntactic-closure))

(defn make-syntactic-closure [env free-vars exp]
  (with-meta
    {:env env, :free-vars (set free-vars), :exp exp}
    {:type ::syntactic-closure}))

(defn identifier? [x]
  (or (symbol? x)
      (and (syntactic-closure? x)
           (symbol? (:exp x)))))

(defn identifier=? [env1 x1 env2 x2]
  (cond (and (symbol? x1) (symbol? x2))
        (= (env/lookup env1 x1) (env/lookup env2 x2))

        (and (identifier? x1) (identifier? x2))
        (= (env/lookup env1 (:exp x1)) (env/lookup env2 (:exp x2)))

        :else false))

(defn capture-syntactic-environment [f]
  (with-meta f {:type ::environment-capturing}))

(defn- environment-capturing? [x]
  (= (type x) ::environment-capturing))

(defn sc-macro-transformer [f]
  (fn [form transformer-env]
    (make-syntactic-closure transformer-env nil (f form *env*))))

;;
;; compiler
;;
(declare compile-exprs compile-special special?)

(defn- compile-symbol [sym]
  (let [var (env/lookup *env* sym)]
    (cond (nil? var) sym
          (var? var) (util/var->qualified-symbol var)
          (class? var) (symbol (.getName var))
          :else var)))

(defn- compile-seq [exp]
  (let [op (first exp)]
    (if (symbol? op)
      (let [m (env/lookup *env* op)]
        (cond (and (var? m) (util/macro? m))
              (compile (apply m exp *env* (rest exp)))

              (fn? m) (compile (m exp))

              (special? op)
              (compile-special exp)

              :else (compile-exprs exp)))
      (compile-exprs exp))))

(defn compile [exp]
  (cond (syntactic-closure? exp)
        (binding [*env* (env/filter-environment (:free-vars exp) *env* (:env exp))]
          (compile (:exp exp)))
        (environment-capturing? exp) (compile (exp *env*))
        (symbol? exp) (compile-symbol exp)
        (and (list? exp) (empty? exp)) '()
        (seq? exp) (compile-seq exp)
        (vector? exp) (vec (compile-exprs exp))
        (map? exp) (reduce (fn [map [key val]]
                             (assoc map (compile key) (compile val)))
                           {}
                           exp)
        (set? exp) (compile-exprs exp)
        :else exp))

(defn- compile-exprs [exprs]
  (doall (map compile exprs)))

;;
;; compilers for special forms
;;
(def ^:private %specials
  '#{def if let* fn* quote do loop* recur var set! new . letfn* clojure.core/import*
     try throw monitor-enter monitor-exit deftype* case* reify*})

(defn- special? [op]
  (%specials op))

(defmulti ^:private compile-special (fn [[op & _]] op))

;;; NB:
;;;  I don't know how (qq (def foo bar)) or (qq (def ~(make-syntactic-closure env nil 'foo) bar))
;;;  should behave.
;;;  For the time being, the first argument of `def' is never renamed.
(defmethod compile-special 'def [exp]
  (let [[_ var init] exp]
    (let [var' (compile var)
          var' (if (namespace var') (symbol (name var')) var')]
      (binding [*env* (env/add-to-environment *env* var' var')]
        `(def ~var' ~(compile init))))))

(defmethod compile-special 'if [exp]
  (let [[_ test then else] exp]
    `(if ~(compile test)
         ~(compile then)
         ~(compile else))))

(defn- compile-nested-inits [names inits env]
  (loop [[name & names] names, [init & inits] inits, ret [], env env]
    (if (nil? name)
      [ret env]
      (recur names
             inits
             (conj ret (binding [*env* env] (compile init)))
             (env/extend-environment env [name])))))

(defmethod compile-special 'let* [exp]
  (let [[_ bindings & body] exp
        bindings' (partition 2 bindings)
        names (map first bindings')
        inits (map second bindings')
        [inits' env'] (compile-nested-inits names inits *env*)]
    (binding [*env* env']
      `(let* ~(vec (interleave (compile-exprs names) inits'))
             ~@(compile-exprs body)))))

(defmethod compile-special 'fn* [exp]
  (let [[_ maybe-name & body] exp
        name (if (symbol? maybe-name) maybe-name nil)
        maybe-args (if name (first body) maybe-name)]
    (if (vector? maybe-args)
      (let [args maybe-args
            body (if name (rest body) body)]
        (binding [*env* (env/extend-environment *env* (if name (cons name args) args))]
         `(fn* ~@(and name [(compile name)]) ~(compile args) ~@(compile-exprs body))))
      (let [body (if name body (cons maybe-name body))]
        (binding [*env* (if name (env/extend-environment *env* [name]) *env*)]
         `(fn* ~@(and name [(compile name)])
               ~@(doall (for [[args & body] body]
                          (binding [*env* (env/extend-environment *env* args)]
                            `(~(compile args)
                              ~@(compile-exprs body)))))))))))

(defmethod compile-special 'quote [exp]
  exp)

(defmethod compile-special 'do [exp]
  (let [[_ & exprs] exp]
    `(do ~@(compile-exprs exprs))))

(defmethod compile-special 'loop* [exp]
  (let [[_ bindings & body] exp
        bindings' (partition 2 bindings)
        names (map first bindings')
        inits (map second bindings')
        [inits' env'] (compile-nested-inits names inits *env*)]
    (binding [*env* env']
     `(loop* ~(vec (interleave (compile-exprs names) inits'))
             ~@(compile-exprs body)))))

(defmethod compile-special 'recur [exp]
  (let [[_ & args] exp]
    `(recur ~@(compile-exprs args))))

(defmethod compile-special 'var [exp]
  (let [[_ exp'] exp]
    `(var ~(compile exp'))))

(defmethod compile-special 'set! [exp]
  (let [[_ var val] exp]
    `(set! ~(compile var)
           ~(compile val))))

(defmethod compile-special 'new [exp]
  (let [[_ class & args] exp]
    `(new ~(compile class)
          ~@(compile-exprs args))))

(defmethod compile-special '. [exp]
  (let [[_ x method-or-field & args] exp]
    `(. ~(compile x)
        ~method-or-field  ;; FIXME: such forms of method (method arg1 arg2 ...) must be accepted
        ~@(compile-exprs args))))

(defmethod compile-special 'letfn* [exp]
  (let [[_ fns & body] exp
        fns' (partition 2 fns)
        fnames (map first fns')
        fexprs (map second fns')]
    (binding [*env* (env/extend-environment *env* fnames)]
      `(letfn* ~(vec (mapcat (fn [[fname fexpr]]
                               [(compile fname) (compile fexpr)])
                             (map vector fnames fexprs)))
               ~@(compile-exprs body)))))

(defmethod compile-special 'clojure.core/import* [exp]
  exp)

(defmethod compile-special 'try [exp]
  (let [[_ & body] exp
        [exprs rest] (split-with #(not (and (seq? %) (= (first %) 'catch))) body)
        [catch-clauses finally-clause]
        (split-with #(not (and (seq? %) (= (first %) 'finally))) rest)]
    `(try ~@(compile-exprs exprs)
          ~@(doall (for [[_ class ename & body] catch-clauses]
                     (binding [*env* (env/extend-environment *env* [ename])]
                       `(catch ~(compile class) ~(compile ename)
                          ~@(compile-exprs body)))))
          ~@(if (empty? finally-clause)
              nil
              (let [[_ & body] (first finally-clause)]
                `((finally ~@(compile-exprs body))))))))

(defmethod compile-special 'throw [exp]
  (let [[_ exp'] exp]
    `(throw ~(compile exp'))))

(defmethod compile-special 'monitor-enter [exp]
  (let [[_ exp'] exp]
    `(monitor-enter ~(compile exp'))))

(defmethod compile-special 'monitor-exit [exp]
  (let [[_ exp'] exp]
    `(monitor-exit ~(compile exp'))))

(defmethod compile-special 'deftype* [exp]
  (let [[_ tag class fields implements interfaces & methods] exp]
    (binding [*env* (env/make-environment (:ns-name *env*) {})]
      `(deftype* ~tag ~class ~fields :implements ~interfaces
         ~@(doall (for [[name args & body] methods]
                    (binding [*env* (env/extend-environment *env* args)]
                      `(~name ~(vec (compile-exprs args))
                              ~@(compile-exprs body)))))))))

(defmethod compile-special 'case* [exp]
  (let [[_ x & rest] exp]
    `(case* ~(compile x) ~@rest)))

(defmethod compile-special 'reify* [exp]
  (let [[_ interfaces & methods] exp]
    `(reify*
       ~(vec (compile-exprs interfaces))
       ~@(doall (for [[name args & body] methods]
                  (binding [*env* (env/extend-environment *env* args)]
                    `(~name ~(vec (compile-exprs args))
                            ~@(compile-exprs body))))))))
