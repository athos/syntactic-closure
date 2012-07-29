(ns identifier
  (:use syntactic-closure.core))

(define-syntax foo []
  (sc-macro-transformer
    (fn [env]
      (capture-syntactic-environment
        (fn [transformer-env]
          (identifier=? transformer-env 'x env 'x))))))

(comment

  [(foo) (let [x 2] (foo))]             ;=> [true false]

  )
