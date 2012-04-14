(ns loop
  (:refer-clojure :exclude [for])
  (:use [syntactic-closure.core :only [define-syntax sc-macro-transformer make-syntactic-closure qq]]))

(define-syntax for [[id init test step] & body]
  (sc-macro-transformer
   (fn [env]
     (letfn [(close [exp free]
               (make-syntactic-closure env free exp))]
       (qq (loop [~id ~(close init [])]
             (when ~(close test [id])
               ~@(map #(close % [id]) body)
               (recur ~(close step [id])))))))))

(comment

  (for [x 0 (< x 3) (+ x 1)]
    (for [y 0 (< y 3) (+ y 1)]
      (println [x y])))

  )
