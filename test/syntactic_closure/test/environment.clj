(ns syntactic-closure.test.environment
  (:use [clojure.test :only [deftest is]])
  (:require [syntactic-closure.environment :as env]))

(in-ns 'foo.bar)
(clojure.core/use 'clojure.core)
(import 'java.util.Calendar)

(defn f [] 0)

(in-ns 'syntactic-closure.test.environment)

(def env (env/make-environment 'foo.bar '{x x_01}))

(deftest lookup
  (is (= (env/lookup env 'x) 'x_01))
  (is (= (env/lookup env 'y) nil))
  (is (= (env/lookup env 'f) #'foo.bar/f))
  (is (= (env/lookup env 'foo.bar/f) #'foo.bar/f))
  (is (= (env/lookup env 'foo.bar/g) nil))
  (is (= (env/lookup env '.methodName) '.methodName))
  (is (= (env/lookup env 'Calendar.) 'java.util.Calendar.))
  (is (= (env/lookup env 'Calendar/AM) 'java.util.Calendar/AM))
  (is (= (env/lookup env 'Calendar) java.util.Calendar)))
