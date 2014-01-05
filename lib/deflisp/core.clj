(def empty?
  (fn [a] (= a ())))

(def inc
  (fn [b] (+ b 1)))

(def map
  (fn [f coll]
    (if (empty? coll) (quote ())
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

(defmacro and
  [cond & conds]
  (list 'if
        cond
        (if (empty? conds)
          'true
          (cons 'and conds))
        cond))

(defmacro test
  [& forms]
  forms)

(defmacro is
  [form]
  (list 'if form
        (do
          (print "Test passed for:" form)
          true)
        (print "The form: " form " returned false")))

(defmacro comment
  [& forms]
  nil)

(defmacro apply
  [f args]
  (cons f (eval args)))

(test
 (reduce (fn [acc cond] (and acc cond))
         (list
          (is (= (first '(1 2 3)) 1))
          (is (= (first ()) nil))
          (is (= (last '(1 2 3)) 3))
          (is (= (last ()) nil))
          (is (empty? ()))
          (is (= (inc 1) 2))
          (is (= '(2 3 4) (map inc '(1 2 3))))
          (is (= '(3 4 5) (map (fn [a] (+ a 2)) '(1 2 3))))
          (is (= 6 (reduce + '(1 2 3) 0)))
          (is (= 6 (apply + (1 2 3))))
          (do
            (def c 5)
            (is (= c 5)))
          (comment
            (is (= c 5)))
          )

         true))
