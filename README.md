# DefLisp

Everyone should implement his own Lisp. I'm not an exception. I'm doing it
just for learning and fun. Don't excpect anything extraordinary here.

What's done so far:

```clj
(def b 100) ;; defining vars
(def a (fn [c] (+ b c))) ;; defining functions
(a 5 7) ;; executing functions
```

What's missing:

  * capturing closures within functions
  * let statements (completely absent), will be implemented before macros, then ditched
    when macros are around
  * list operations (first, rest, car, cdr)

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
