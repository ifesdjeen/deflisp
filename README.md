# DefLisp

Everyone should implement his own Lisp. I'm not an exception. I'm doing it
just for learning and fun. Don't excpect anything extraordinary here.

What's done so far:

```clj
(def b 100) ;; defining vars
(def a (fn [c] (+ b c))) ;; defining functions
(a 5 7) ;; executing functions

(first '(1 2 3 4))
;; => 1

(last '(1 2 3 4))
;; => 4

(rest '(1 2 3 4))
;; => '(2 3 4)

(conj (quote (1 2 3)) 4)
;; => '(1 2 3 4)

(cons 1 (quote (2 3 4)))
;; => '(1 2 3 4)


(def empty?
  (fn [a] (= a (quote ()))))

(def inc
  (fn [b]
    (+ b 1)))

(def map
  (fn [f coll]
    (if (empty? coll)
      (quote ())
      (cons (f (first coll))
    (map f (next coll))))))

(map inc (quot (1 2 3)))
;; => (2,3,4)

(def reduce
  (fn [f coll acc]
    (if (empty? coll)
      acc
      (reduce f (next coll) (f acc (first coll))))))
```

In words:
  * `if` conditionals
  * basic list operations (`first`, `next`, `last`, `cons`, `conj`)
  * equality checks (`=`)
  * function definition
  * recursion (yeah, tail-call optimised, hence Haskell)

What's missing:

  * capturing closures within functions
  * let statements (completely absent), will be implemented before macros, then ditched
    when macros are around

Lists are not cons cells, so it's not a real lisp, but I don't care.



## Usage

Try avoiding using it

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

Copyright (c) Alex Petrov

Double licensed under the Eclipse Public License or the Apache Public License 2.0.


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/ifesdjeen/deflisp/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

