(ns syntactic-closure.test.core
  (:refer-clojure :exclude [compile])
  (:use [clojure.test :only [deftest is]])
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
  (is (= (core/compile env '(.method x y)) '(.method x_01 y_02))))
