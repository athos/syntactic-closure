(ns close
  (:use [syntactic-closure :only [define-syntax sc-macro-transformer make-syntactic-closure qq]]))

(defn foo [x]
  x)

(define-syntax bar [x]
  (sc-macro-transformer
    (fn [env]
      (qq (foo ~(make-syntactic-closure env nil x))))))

(comment

  (ns baz
    (:use [close :only [bar]]))
  (defn foo [x]
    (* x x))

  (macroexpand '(bar (foo 4))) ;=> (close/foo (baz/foo 4))

  )
