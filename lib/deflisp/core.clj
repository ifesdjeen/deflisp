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

(defmacro or
  [cond & conds]
  (list 'if
        cond
        cond
        (if
            (empty? conds)
          'false
          (cons 'or conds))))

(comment
  (defmacro and
    [cond & conds]
    '(if ~cond
       (if (empty? ~conds)
         true
         (and conds))
       cond)))

(defmacro test
  [& forms]
  forms)

(defmacro is
  [msg form]
  '(if ~form
     (do
       (print "Test passed for:" ~form)
       true)
     (print "Test for" ~msg " doesnt work")))

(defmacro apply
  [f args]
  (cons f (eval args)))

(is "First" (= (first '(1 2 3)) 1))
(is "First  " (= (first ()) nil))
(is "Last  " (= (last '(1 2 3)) 3))
(is "Last  " (= (last ()) nil))
(is "Empty " (empty? ()))
(is "Inc  " (= (inc 1) 2))
(is "Map  " (= '(2 3 4) (map inc '(1 2 3))))
(is "Map  with anon function" (= '(3 4 5) (map (fn [a] (+ a 2)) '(1 2 3))))
(is "Reduce  " (= 6 (reduce + '(1 2 3) 0)))
(is "Aplly  " (= 6 (apply + (1 2 3))))
(do
  (def c 5)
  (is "Do closure" (= c 5)))

(is "Do closure definitions" (= c 5))
