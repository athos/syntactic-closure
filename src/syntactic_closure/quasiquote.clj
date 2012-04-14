(in-ns 'syntactic-closure.core)

;;
;; self-made quasiquote, because Clojure's syntax-quote meddles too much
;;
(def ^:private UNQUOTE 'clojure.core/unquote)
(def ^:private UNQUOTE-SPLICING 'clojure.core/unquote-splicing)

(defn- unquote? [x]
  (and (seq? x) (= (first x) UNQUOTE)))

(defn- unquote-splicing? [x]
  (and (seq? x) (= (first x) UNQUOTE-SPLICING)))

(defn expand [xxs depth]
  (cond (and (unquote? xxs) (= depth 0)) (second xxs)

        (seq? xxs)
        (let [[x & [x' :as xs]] xxs]
          (cond (and (unquote-splicing? x) (= depth 0))
                `(concat ~(second x) ~(expand xs depth))

                (not (nil? x))
                (cond (= x 'qq)
                      `(list '~'qq ~(expand x' (inc depth)))

                      (and (= x UNQUOTE) (> depth 0))
                      `(list '~UNQUOTE ~(expand x' (dec depth)))

                      (and (= x UNQUOTE-SPLICING) (> depth 0))
                      `(list '~UNQUOTE-SPLICING ~(expand x' (dec depth)))

                      :else `(cons ~(expand x depth) ~(expand xs depth)))))

        (vector? xxs)
        `(vec ~(expand (seq xxs) depth))

        (map? xxs)
        `(apply conj {} (map vec (partition 2 ~(expand (apply concat xxs) depth))))

        :else `'~xxs))

(defmacro qq [x]
  (expand x 0))
