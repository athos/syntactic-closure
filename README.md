# syntactic-closure

`syntactic-closure` is a Clojure library that provides some facilities
to define hygienic macros with syntactic closures.
It aims to implement a hygienic macro system interoperable with Clojure's
macro system.

For details about syntactic closures, see [Syntactic Closures](ftp://publications.ai.mit.edu/ai-publications/pdf/AIM-1049.pdf) or [Chicken Scheme's wiki page](http://wiki.call-cc.org/eggref/3/syntactic-closures).

**Note**: `syntactic-closure` is still of alpha quality.

## Usage

Add the following to your project.clj dependencies:

```clojure
[syntactic-closure "0.1.0"]
```

Use via:

```clojure
(use 'syntactic-closure.core)
```

or, for the shorthand, use via:

```clojure
(use 'syntactic-closure)
```

## About

As on some Scheme implementations, you can define hygienic macros
using syntactic closures, like this:

```clojure
(use 'syntactic-closure.core)

(define-syntax let1 [name init & body]
  (sc-macro-transformer
    (fn [env]
      (quasiquote
        (let [~name ~(make-syntactic-closure env nil init)]
          ~@(map #(make-syntactic-closure env [name] %) body))))))

(let1 x 10 (* x x))
```

In this case, the input name `x` is automatically renamed.

```clojure
(macroexpand '(let1 x 10 (* x x)))
;=> (let* [x403 10] (clojure.core/* x403 x403))
```

Using syntactic closures, you can also define anaphoric macros.

```clojure
(define-syntax aif [test then else]
  (sc-macro-transformer
    (fn [env]
      (let [it ~(make-syntactic-closure env nil test)]
        (if it
          ~(make-syntactic-closure env '[it] then)
          ~(make-syntactic-closure env nil else))))))
```


Since Scheme-like interfaces are somehow verbose, `syntactic-closure`
namespace provides a simple shorthand for them (, though it is subject to change).

```clojure
(use 'syntactic-closure)

(defsyntax let1 [name init & body]
  (qq (let [~name ~^:? init]
        ~@^{:? name} body)))

(defsyntax aif [test then else]
  (qq (let [it ~^:? test]
        (if it
          ~^{:? 'it} then
          ~^:? else))))
```

For details about the shorthand, see Shorthand section below.

For more examples, see example code in the `/examples` directory.
Each example includes both verbose and concise versions of code.

## Shorthand
`syntactic-closure` namespace provides `qq` macro, which is almost the same
as `quasiquote` except that in `qq` form, the following shorthand can be used.

* `~^:? foo` is equivalent to `~(make-syntactic-closure *env* nil foo)`
* For any symbol `id`, `~^{:? id} foo` is equivalent to `~(make-syntactic-closure *env* [id] foo)`
* For any list of symbols `ids`, `~^{:? ids} foo` is equivalent to `~(make-syntactic-closure *env* ids foo)`
* `~@^:? foo` is equivalent to `~@(map (bound-fn [x#] (make-syntactic-closure *env* nil x#)) foo)`
* For any symbol `id`, `~@^{:? id} foo` is equivalent to `~@(map (bound-fn [x#] (make-syntactic-closure *env* [id] x#)) foo)`
* For any list of symbols `ids`, `~@{:? ids} foo` is equivalent to `~@(map (bound-fn [x#] (make-syntactic-closure *env* ids x#)) foo)`

## Resources

* [Syntactic Closures](ftp://publications.ai.mit.edu/ai-publications/pdf/AIM-1049.pdf)
* [A Syntactic Closures Macro Facility](http://groups.csail.mit.edu/mac/ftpdir/scheme-reports/synclo.ps)

## License

Copyright (C) 2012 OHTA Shogo

Distributed under the Eclipse Public License, the same as Clojure.
