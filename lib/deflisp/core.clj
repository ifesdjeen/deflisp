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

(print (empty? ()))
(print (= (inc 1) 2))
(print (= '(2 3 4) (map inc '(1 2 3))))
(print (= 6 (reduce + '(1 2 3) 0)))
