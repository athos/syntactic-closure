(ns close
  (:use [syntactic-closure :only [defsyntax qq]]))

(defn foo [x]
  x)

(defsyntax bar [x]
  (qq (foo ~^:? x)))

(comment

  (ns baz
    (:use [close :only [bar]]))
  (defn foo [x]
    (* x x))

  (macroexpand '(bar (foo 4))) ;=> (close/foo (baz/foo 4))

  )
