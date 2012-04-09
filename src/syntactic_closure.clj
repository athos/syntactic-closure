(ns syntactic-closure
  (:refer-clojure :exclude [compile]))

;;
;; syntactic environment
;;
(declare error)

(defn- toplevel-env []
  (ns-map *ns*))

(defn- lookup [env name]
  (or (env name)
      ((toplevel-env) name)))

(defn- extend-environment [env ids]
  (reduce (fn [env id] (assoc env id (gensym id))) env ids))

(defn- filter-environment [names names-env else-env]
  (reduce (fn [env [name val]]
            (if (names name)
              (assoc env name val)
              env))
          else-env
          names-env))

;;
;; syntactic closures
;;
(declare add-meta compile)

(defn- syntactic-closur-ize [f]
  (with-meta f {:type ::syntactic-closure}))

(defn make-syntactic-closure [env free-vars exp]
  (let [free-vars' (set free-vars)]
    (syntactic-closur-ize
      (fn [free-names-env]
        (compile (filter-environment free-vars' free-names-env env) exp)))))

(defn syntactic-closure? [x]
  (if-let [m (meta x)]
    (= (:type m) ::syntactic-closure)))

(defmacro sc-macro-transformer [f]
  (if-let [env (::env (meta &form))]
    `(make-syntactic-closure {} nil (~f '~env))
    `(let [env# (reduce (fn [env# [name# val#]]
                          (assoc env# name# name#))
                        {}
                        ~'&env)]
       (compile env# (make-syntactic-closure {} nil (~f env#))))))

(defn capture-syntactic-environment [f]
  (syntactic-closur-ize f))

;;
;; compiler
;;
(declare compile-exprs special? compile-special var->qualified-symbol macro?)

(defn- compile-symbol [env sym]
  (if (namespace sym)
    sym
    (let [var (lookup env sym)]
      (cond (nil? var) (error "unknown identifier: " sym)
            (var? var) (var->qualified-symbol var)
            :else var))))

(defn- compile-seq [env exp]
  (let [op (first exp), m (lookup env op)]
    (cond (and (var? m) (macro? m))
          (compile env
                   (apply m (add-meta exp {::env env}) env (rest exp)))

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
;; compile functions for special forms
;;
(def ^:private %specials
  '#{def if let* fn* quote do loop* recur var})

(defn- special? [op]
  (%specials op))

(defmulti ^:private compile-special (fn [env [op & _]] op))

;; FIXME:
;;  DEF's init expr should be evaluated in the environment extended to include
;;  defining variable.
(defmethod compile-special 'def [env exp]
  (let [[_ name init] exp]
    `(def ~(compile env name)
          ~(compile env init))))

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
        env' (extend-environment env names)]
    `(let* ~(vec (interleave (compile-exprs env' names)
                             (compile-exprs env' inits)))
           ~@(compile-exprs env' body))))

;; FIXME:
;;  Forms like (fn* foo [arg ...] body ...) are not supported.
(defmethod compile-special 'fn* [env exp]
  (let [[_ args & body] exp
        env' (extend-environment env args)]
    `(fn* ~(vec (compile-exprs env' args))
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
        env' (extend-environment env names)]
    `(loop* ~(vec (interleave (compile-exprs env' names)
                              (compile-exprs env' inits)))
            ~@(compile-exprs env' body))))

(defmethod compile-special 'recur [env exp]
  (let [[_ & args] exp]
    `(recur ~@(compile-exprs env args))))

(defmethod compile-special 'var [env exp]
  (let [[_ var] exp]
    `(var ~(compile-symbol var))))

;;
;; self-made quasiquote, because Clojure's syntax-quote meddles too much
;;
(def ^:private UNQUOTE 'clojure.core/unquote)
(def ^:private UNQUOTE-SPLICING 'clojure.core/unquote-splicing)

(defn- unquote? [x]
  (and (seq? x) (= (first x) UNQUOTE)))

(defn- unquote-splicing? [x]
  (and (seq? x) (= (first x) UNQUOTE-SPLICING)))

(defn expand [xxs depth]
  (cond (and (unquote? xxs) (= depth 0)) (second xxs)
        
        (seq? xxs)
        (let [[x & [x' :as xs]] xxs]
          (cond (and (unquote-splicing? x) (= depth 0))
                `(concat ~(second x) ~(expand xs depth))

                (not (nil? x))
                (cond (= x 'qq)
                      `(list '~'qq ~(expand x' (inc depth)))

                      (and (= x UNQUOTE) (> depth 0))
                      `(list '~UNQUOTE ~(expand x' (dec depth)))

                      (and (= x UNQUOTE-SPLICING) (> depth 0))
                      `(list '~UNQUOTE-SPLICING ~(expand x' (dec depth)))

                      :else `(cons ~(expand x depth) ~(expand xs depth)))))
        
        (vector? xxs)
        `(vec ~(expand (seq xxs) depth))

        (map? xxs)
        `(apply conj {} (map vec (partition 2 ~(expand (apply concat xxs) depth))))

        :else `'~xxs))

(defmacro qq [x]
  (expand x 0))

;;
;; miscellaneous
;;
(defn- error [& msgs]
  (throw (Exception. (apply str msgs))))

(defn- var->qualified-symbol [^clojure.lang.Var var]
  (let [^clojure.lang.Namespace ns (.ns var)]
    (symbol (str (.name ns)) (str (.sym var)))))

(defn- macro? [var]
  (if-let [m (meta var)]
    (:macro m)))

(defn- add-meta [x m]
  (if (meta x)
    (vary-meta x #(into % m))
    (with-meta x m)))

;;
;; Examples
;;
(comment

  (defmacro let1 [var val & body]
    (sc-macro-transformer
      (fn [env]
        (qq (let [~var ~(make-syntactic-closure env nil val)]
              ~@(map #(make-syntactic-closure env [var] %) body))))))



)