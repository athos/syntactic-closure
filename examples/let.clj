(ns let
  (:use [syntactic-closure.core :only [define-syntax sc-macro-transformer make-syntactic-closure quasiquote]]
        [syntactic-closure :only [defsyntax qq]]))

;; verbose version
(define-syntax my-let' [bindings & body]
  (sc-macro-transformer
    (fn [env]
      (let [bindings' (partition 2 bindings)
            names (map first bindings')
            inits (map second bindings')]
        (quasiquote
          ((fn ~(vec names)
             ~@(map #(make-syntactic-closure env names %) body))
           ~@(map #(make-syntactic-closure env nil %) inits)))))))

;; concise version
(defsyntax my-let [bindings & body]
  (let [bindings' (partition 2 bindings)
        names (map first bindings')
        inits (map second bindings')]
    (qq ((fn ~(vec names) ~@^{:? names} body)
         ~@^:? inits))))

(comment
  You can also use make-syntactic-closure within qq, like quasiquote.
  If you want to get the syntactic environment where the macro is called,
  you'll get it by refering to *env*.

  (defsyntax my-let [bindings & body]
    (let [bindings' (partition 2 bindings)
          names (map first bindings')
          inits (map second bindings')
          env *env*]
      (qq ((fn ~(vec names) ~@(map #(make-syntactic-closure env names %) body))
           ~@(map #(make-syntactic-closure env nil %) inits)))))
  )

(comment

  (def x 101)
  (my-let [x 1, y x]
    [x y])
  ;=> [1 101]
  
)
