(in-ns 'syntactic-closure)

(use '[syntactic-closure.util :only [error var->qualified-symbol add-meta macro?]]
     '[syntactic-closure.environment :only [lookup]])

(declare compile compile-exprs syntactic-closure? compile-special special?)

(defn- compile-symbol [env sym]
  (if (namespace sym)
    sym
    (let [var (lookup env sym)]
      (cond (nil? var) (error "unknown identifier: " sym)
            (var? var) (var->qualified-symbol var)
            :else var))))

(defn- compile-seq [env exp]
  (let [op (first exp), m (lookup env op)]
    (cond (and (var? m) (macro? m))
          (compile env (apply m (add-meta exp {::env env}) env (rest exp)))

          (special? op)
          (compile-special env exp)

          :else (compile-exprs env exp))))

(defn- compile-syntactic-closure [env sc]
  (sc env))

(defn compile [env exp]
  (cond (syntactic-closure? exp) (compile-syntactic-closure env exp)
        (symbol? exp) (compile-symbol env exp)
        (and (list? exp) (empty? exp)) '()
        (seq? exp) (compile-seq env exp)
        (vector? exp) (vec (compile-exprs env exp))
        (map? exp) (reduce (fn [map [key val]]
                             (assoc map (compile env key) (compile env val)))
                           {}
                           exp)
        (set? exp) (compile-exprs env exp)
        :else exp))

(defn- compile-exprs [env exprs]
  (map #(compile env %) exprs))
