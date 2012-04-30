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

(defn expand [xxs depth kwd on-unquote on-unquote-sp]
  (letfn [(rec [xxs depth]
            (cond (and (unquote? xxs) (= depth 0)) (on-unquote (second xxs))

                  (and (seq? xxs) (not (empty? xxs)))
                  (let [[x & [x' :as xs]] xxs]
                    (if (and (unquote-splicing? x) (= depth 0))
                      `(concat ~(on-unquote-sp (second x)) ~(rec xs depth))
                      (cond (= x kwd)
                            `(list '~kwd ~(rec x' (inc depth)))

                            (and (= x UNQUOTE) (> depth 0))
                            `(list '~UNQUOTE ~(rec x' (dec depth)))

                            (and (= x UNQUOTE-SPLICING) (> depth 0))
                            `(list '~UNQUOTE-SPLICING ~(rec x' (dec depth)))

                            :else `(cons ~(rec x depth) ~(rec xs depth)))))

                  (vector? xxs)
                  `(vec ~(rec (seq xxs) depth))

                  (map? xxs)
                  `(apply conj {} (map vec (partition 2 ~(rec (apply concat xxs) depth))))

                  :else `'~xxs))]
    (rec xxs depth)))

(defmacro quasiquote [x]
  (expand x 0 'quasiquote identity identity))
