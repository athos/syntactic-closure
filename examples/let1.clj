(ns let1
  (:use [syntactic-closure.core :only [define-syntax sc-macro-transformer make-syntactic-closure qq]]))

(define-syntax let1 [var val & body]
  (sc-macro-transformer
    (fn [env]
      (qq (let [~var ~(make-syntactic-closure env nil val)]
            ~@(map #(make-syntactic-closure env [var] %) body))))))
