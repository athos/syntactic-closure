(ns loop
  (:refer-clojure :exclude [for])
  (:use [syntactic-closure.core :only [define-syntax sc-macro-transformer make-syntactic-closure quasiquote]]
        [syntactic-closure :only [defsyntax qq]]))

;; verbose version
(define-syntax for' [[id init test step] & body]
  (sc-macro-transformer
   (fn [env]
     (letfn [(close [exp free]
               (make-syntactic-closure env free exp))]
       (quasiquote
         (loop [~id ~(close init [])]
           (when ~(close test [id])
             ~@(map #(close % [id]) body)
             (recur ~(close step [id])))))))))

;; concise version
(defsyntax for [[id init test step] & body]
  (qq (loop [~id ~^:? init]
        (when ~^{:? id} test
          ~@^{:? id} body
          (recur ~^{:? id} step)))))

(comment
  You can specify to ^:? either a symbol or a list of symbols.
  So, you can rewrite the for macro above like the following:

  (defsyntax for [[id init test step] & body]
    (qq (loop [~id ~^:? init]
          (when ~^{:? [id]} test
            ~@^{:? [id]} body
            (recur ~^{:? [id]} step)))))
  
  )

(comment

  (for [x, 0, (< x 3), (+ x 1)]
    (for [y, 0, (< y 3), (+ y 1)]
      (println [x y])))

  )
