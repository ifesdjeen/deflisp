(defmacro comment
  [& forms]
  nil)

(def empty?
  (fn [a] (= a ())))

(def inc
  (fn [b] (+ b 1)))

(def map
  (fn [f coll]
    (if (empty? coll)
      ()
      (cons (f (first coll))
            (map f (next coll))))))

(def reduce
  (fn [f coll acc]
    (if (empty? coll)
      acc
      (reduce f (next coll)
              (f acc (first coll))))))

(comment
  (defmacro or
    [cond & conds]
    '(if ~cond
       ~cond
       (if (empty? ~conds)
         'false
         (or ~(first conds) ~(next conds)))))

  (defmacro and
    [cond & conds]
    '(if ~cond
       (if ~(empty? conds)
         true
         (and ~(first conds) ~(next conds)))
       ~cond)))

(defmacro test
  [& forms]
  forms)

(defmacro is
  [msg form]
  '(if ~form
     (do
       (print "Test passed for:" ~msg)
       true)
     (print "Test for" ~msg " doesnt work")))

(defmacro apply
  [f args]
  (cons f (eval args)))

(is "first" (= (first '(1 2 3)) 1))
(is "first with empty list  " (= (first ()) nil))
(is "last" (= (last '(1 2 3)) 3))
(is "last with empty list" (= (last ()) nil))
(is "empty with empty list" (empty? ()))
(is "inc" (= (inc 1) 2))
(is "map" (= '(2 3 4) (map inc '(1 2 3))))
(is "map with anon function" (= '(3 4 5) (map (fn [a] (+ a 2)) '(1 2 3))))
(is "reduce  " (= 6 (reduce + '(1 2 3) 0)))
(is "aplly  " (= 6 (apply + (1 2 3))))
(do
  (def c 5)
  (is "do closure" (= c 5)))

(comment (is "and" (= (and 1 false 2) 1))
         (is "or" (= (or 1 2 false) 1))
         (is "or" (= (or false 1 2) 1))

         (is "and" (= (and 1 2 3) 1))

)
