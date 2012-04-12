(ns push
  (:use [syntactic-closure :only [define-syntax sc-macro-transformer make-syntactic-closure qq]]))

(define-syntax push! [x v]
  (sc-macro-transformer
    (fn [env]
      (let [x' (make-syntactic-closure env nil x)]
        (qq (set! ~x' (conj ~x' ~(make-syntactic-closure env nil v))))))))

(comment
  (def ^:dynamic x)

  (binding [x []]
    (push! x 0)
    (push! x 1)
    (push! x 2)
    x) ;=> [0 1 2]
  )
