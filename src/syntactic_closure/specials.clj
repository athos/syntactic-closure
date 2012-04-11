(in-ns 'syntactic-closure)

(use '[syntactic-closure.environment :only [extend-environment add-to-environment]])

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
          env' (add-to-environment env var' var')]
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
  exp)
