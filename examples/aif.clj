(ns aif
  (:use [syntactic-closure.core :only [define-syntax sc-macro-transformer make-syntactic-closure quasiquote]]
        [syntactic-closure :only [defsyntax qq]]))

;; verbose version
(define-syntax aif' [test then else]
  (sc-macro-transformer
    (fn [env]
      (quasiquote
        (let [it ~(make-syntactic-closure env nil test)]
          (if it
            ~(make-syntactic-closure env '[it] then)
            ~(make-syntactic-closure env nil else)))))))

(define-syntax awhen' [test then]
  (sc-macro-transformer
    (fn [env]
      (quasiquote
        (aif' ~(make-syntactic-closure env nil test)
          ~(make-syntactic-closure env '[it] then)
          nil)))))

;; concise version
(defsyntax aif [test then else]
  (qq (let [it ~^:? test]
        (if it
          ~^{:? 'it} then
          ~^:? else))))

(defsyntax awhen [test then]
  (qq (aif ~^:? test
        ~^{:? 'it} then
        nil)))

(comment

  (def m {:x "42"})
  (aif (m :x)
    (Integer/parseInt it)
    0)
  ;=> 42

  (awhen (m :x)
    (prn it))   ; prints "42"
  ;=> nil
  
)
