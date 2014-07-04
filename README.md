# DefLisp

Everyone should implement his own Lisp. I'm not an exception. I'm doing it
just for learning and fun. Don't excpect anything extraordinary here.

[Here's a little blog post on the subject](http://coffeenco.de/articles/write_you_lisp_in_haskell.html), that explains main concepts that were used here.

I've tried my best to stick to Clojure syntax. Actually, most of the "core"
implementation was first written in Clojure, afterwards it ran smoothly on deflisp.
Because I've enjoyed working on deflisp so much, you may expect it to be under
active development, but all I'm doing here is for the joy of programming. I'm
somehow certain that there are better lisps out there.

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
  * macros

What's missing:

  * let statements (completely absent)

Big plans:

  * pattern matching
  * algebraic data types

Lists are not cons cells, so it's not a real lisp, but I don't care.



## Usage

Try to avoid using it.

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Emacs Development

Usually everything should work just fine, except for you have to set a working directory sometimes:

```
:set -iFULL_PATH_TO_deflisp/src
```

## Contributing

Copyright (c) Alex Petrov

Double licensed under the Eclipse Public License or the Apache Public License 2.0.


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/ifesdjeen/deflisp/trend.png)](https://bitdeli.com/free "Bitdeli Badge")
