(ns syntactic-closure
  (:require [syntactic-closure.core :as sc]))

;;
;; Note:
;;  The macros provided in this namespace are subject to change.
;;
(def ^:dynamic *env*)

(defmacro defsyntax [name args & body]
  `(sc/define-syntax ~name ~args
     (sc/sc-macro-transformer
       (fn [env#]
         (binding [*env* env#]
           ~@body)))))

(defn free-vars [m]
  (cond (true? m) nil
        (coll? m) m
        :else [m]))

(defmacro qq [x]
  (sc/expand x 0 'qq
    (fn [x]
      (if-let [m (:? (meta x))]
        `(sc/make-syntactic-closure *env* (free-vars ~m) ~x)
        x))
    (fn [x]
      (if-let [m (:? (meta x))]
        `(map (bound-fn [x#] (sc/make-syntactic-closure *env* (free-vars ~m) x#)) ~x)
        x))))
