(ns syntactic-closure.test.core
  (:refer-clojure :exclude [compile])
  (:use [clojure.test :only [deftest is are]])
  (:require [syntactic-closure.core :as core]
            [syntactic-closure.environment :as env]))

(in-ns 'foo.baz)
(clojure.core/use 'clojure.core)
(import 'java.util.Calendar)

(defn f [] 0)

(in-ns 'syntactic-closure.test.core)

(def env (env/make-environment 'foo.baz '{x x_01, y y_02}))

(deftest compile
  (is (= (core/compile env 'x) 'x_01))
  (is (= (core/compile env 'z) 'z))
  (is (= (core/compile env '(f x y)) '(foo.baz/f x_01 y_02)))
  (is (= (core/compile env '(g x y)) '(g x_01 y_02)))
  (is (= (core/compile env '(foo.baz/f x y)) '(foo.baz/f x_01 y_02)))
  (is (= (core/compile env '(foo.baz/g x y)) '(foo.baz/g x_01 y_02)))
  (is (= (core/compile env 'Calendar) 'java.util.Calendar))
  (is (= (core/compile env '(new Calendar x y)) '(new java.util.Calendar x_01 y_02)))
  (is (= (core/compile env '(Calendar. x y)) '(java.util.Calendar. x_01 y_02)))
  (is (= (core/compile env '(. Calendar getInstance)) '(. java.util.Calendar getInstance)))
  (is (= (core/compile env '(Calendar/getInstance)) '(java.util.Calendar/getInstance)))
  (is (= (core/compile env '(. x method y)) '(. x_01 method y_02)))
  (is (= (core/compile env '(.method x y)) '(.method x_01 y_02)))
  (is (= (core/compile env '((fn* [] (f x)))) '((fn* [] (foo.baz/f x_01))))))

(deftest compile-def
  (are [expr expanded] (= (core/compile env expr) expanded)
    '(def a x)
    '(def a x_01)

    '(def a (fn* [] a))
    '(def a (fn* [] a))))

(deftest compile-if
  (are [expr expanded] (= (core/compile env expr) expanded)
    '(if (f 0) x y)
    '(if (foo.baz/f 0) x_01 y_02)))

(deftest compile-let*
  (let [[op [name val] [f x]] (core/compile env '(let* [x x] (f x)))]
    (is (and (= op 'let*)
             (= name x)
             (not= name 'x_01)
             (= val 'x_01)
             (= f 'foo.baz/f)))))

(deftest compile-fn*
  (let [[op [name] [f x]] (core/compile env '(fn* [x] (f x)))]
    (is (and (= op 'fn*)
             (= name x)
             (not= name 'x)
             (not= name 'x_01)
             (= f 'foo.baz/f))))
  (let [[op [[name] [f x]]] (core/compile env '(fn* ([x] (f x))))]
    (is (and (= op 'fn*)
             (= name x)
             (not= name 'x)
             (not= name 'x_01)
             (= f 'foo.baz/f))))
  (let [[op fname [name] [f x]] (core/compile env '(fn* f [x] (f x)))]
    (is (and (= op 'fn*)
             (= fname f)
             (not= f 'f)
             (not= f 'foo.baz/f)
             (= name x)
             (not= name 'x)
             (not= name 'x_01))))
  (let [[op fname [[name] [f x]]] (core/compile env '(fn* f ([x] (f x))))]
    (is (and (= op 'fn*)
             (= fname f)
             (not= f 'f)
             (not= f 'foo.baz/f)
             (= name x)
             (not= name 'x)
             (not= name 'x_01)))))

(deftest compile-quote
  (is (= (core/compile env '(quote (def f x))) '(quote (def f x)))))

(deftest compile-do
  (is (= (core/compile env '(do (f x) y)) '(do (foo.baz/f x_01) y_02))))

(deftest compile-loop*
  (let [[_ [name val] [f x]] (core/compile env '(loop* [x x] (f x)))]
    (is (and (= name x)
             (= val 'x_01)
             (= f 'foo.baz/f)))))

(deftest compile-recur
  (is (= (core/compile env '(recur (f x))) '(recur (foo.baz/f x_01)))))

(deftest compile-var
  (is (= (core/compile env '(var f)) '(var foo.baz/f))))

(deftest compile-set!
  (is (= (core/compile env '(set! y (f x))) '(set! y_02 (foo.baz/f x_01)))))

(deftest compile-letfn*
  (let [[op [f1 [fn1 f2 [x1] [f3 [g4 x2]]], g1 [fn2 g2 [x3] [g3 [f4 x4]]]] [f5 [g5 x]]]
        (core/compile env '(letfn* [f (fn* f [x] (f (g x))), g (fn* g [x] (g (f x)))] (f (g x))))]
    (is (and  (= op 'letfn*)
              (= fn1 fn2 'fn*)
              (= f1 f4 f5)
              (not= f1 'f)
              (not= f1 'foo.baz/f)
              (= f2 f3)
              (= g1 g4 g5)
              (not= g1 'g)
              (= g2 g3)
              (= x1 x2)
              (not= x1 'x)
              (not= x1 'x_01)
              (= x3 x4)
              (not= x3 'x)
              (not= x3 'x_01)
              (= x 'x_01)))))

(deftest compile-try
  (let [[op1 [f1 x] [f2 y] [op2 class name [f3 e]] [op3 [f4 x']]]
        (core/compile env
          '(try (f x) (f y) (catch Exception x (f x)) (finally (f x))))]
    (is (and (= op1 'try)
             (= op2 'catch)
             (= op3 'finally)
             (= f1 f2 f3 f4 'foo.baz/f)
             (= x x' 'x_01)
             (= y 'y_02)
             (= class 'java.lang.Exception)
             (= name e)
             (not= name 'x)
             (not= name 'x_01)))))

(deftest compile-throw
  (is (= (core/compile env '(throw (new Exception x)))
         '(throw (new java.lang.Exception x_01)))))

(deftest compile-monitor-enter
  (is (= (core/compile env '(monitor-enter x))
         '(monitor-enter x_01))))

(deftest compile-monitor-exit
  (is (= (core/compile env '(monitor-exit x))
         '(monitor-exit x_01))))

(deftest compile-deftype*
  (let [[op tag class [x1] implements [interface] [invoke [t] [f x2]]]
        (core/compile env
          '(deftype* Foo foo.Foo [x] :implements [clojure.lang.IFn]
             (invoke [this] (f x))))]
    (is (and (= op 'deftype*)
             (= tag 'Foo)
             (= class 'foo.Foo)
             (= x1 x2 'x)
             (= implements :implements)
             (= interface 'clojure.lang.IFn)
             (= invoke 'invoke)
             (not= t 'this)
             (= f 'foo.baz/f)))))

(deftest compile-case*
  ;; nothing to be tested so far
  )

(deftest compile-reify*
  (let [[op [i] [f1 [t1] [f2 x2]] [f3 [t2 x3] [f4 x4]]]
        (core/compile env
          '(reify*
             [clojure.lang.IFn]
             (invoke [this] (f x))
             (invoke [this x] (f x))))]
    (is (and (= op 'reify*)
             (= i 'clojure.lang.IFn)
             (= f1 f3 'invoke)
             (not= t1 'this)
             (= f2 'foo.baz/f)
             (= x2 'x_01)
             (not= t2 'this)
             (= x3 x4)
             (not= x3 'x)
             (not= x3 'x_01)
             (= f4 'foo.baz/f)))))

(deftest compile-syntactic-closure
  (let [env' (env/make-environment 'foo.baz '{x x_777})]
    (is (= (core/compile env (core/make-syntactic-closure env' nil 'x))
           'x_777))
    (is (= (core/compile env (core/make-syntactic-closure env' nil 'y))
           'y))
    (is (= (core/compile env (core/make-syntactic-closure env' '(x) 'x))
           'x_01))
    (is (= (core/compile env (core/make-syntactic-closure env' '(y) 'y))
           'y_02))))
