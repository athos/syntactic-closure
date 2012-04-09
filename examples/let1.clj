(ns let1
  (:use [syntactic-closure :only [sc-macro-transformer make-syntactic-closure qq]]))

(defmacro let1 [var val & body]
  (sc-macro-transformer
    (fn [env]
      (qq (let [~var ~(make-syntactic-closure env nil val)]
            ~@(map #(make-syntactic-closure env [var] %) body))))))
