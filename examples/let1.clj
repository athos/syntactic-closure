(ns let1
  (:use [syntactic-closure :only [defsyntax qq]]))

(defsyntax let1 [var val & body]
  (qq (let [~var ~^:? val]
        ~@^{:? var} body)))
