(ns solutions)

(defmacro unit-test [problem & tests]
  `(run-tests ~problem '~tests)) 

(defn- execute-one-test [test]
  (try 
    (if (eval test) 
      (do (println "passed" test) true) 
      (do (println "failed" test) false)) 
    (catch Throwable e 
      (do (println "failed with" e test) false))))

(defn- execute-tests [tests]
  (reduce
    (fn [res test]
      (+ 
        (if (execute-one-test test) 1 0)
        res))
    0
    tests))

(defn- run-tests [problem tests]
  (do
    (println "Executing" problem)
    (println (execute-tests tests) "/" (count tests) "passed")))

;;#52
;;(= [2 4] (let [[a b c d e f g] (range)] my-flip-args))
(unit-test 
  "problem52"
  (= [2 4] (let [[a b c d e f g] (range)] [c e])))

;;#44
;; Write a function which can rotate a sequence in either direction.
(def my-rotate 
  (fn [n l]
    (let [addlast (fn f [e l]
                    (if (empty? l) 
                      (list e)
                      (cons (first l) (f e (rest l)))))]
      (cond
        (= n 0) l
        (< n 0) (recur (inc n) (cons (last l) (butlast l)))
        (> n 0) (recur (dec n) (addlast (first l) (rest l)))))))
  
(unit-test 
  "problem44"
  (= (my-rotate 2 [1 2 3 4 5]) '(3 4 5 1 2))
  (= (my-rotate -2 [1 2 3 4 5]) '(4 5 1 2 3))
  (= (my-rotate 6 [1 2 3 4 5]) '(2 3 4 5 1))
  (= (my-rotate 1 '(:a :b :c)) '(:b :c :a))
  (= (my-rotate -4 '(:a :b :c)) '(:c :a :b)))

;;#55
;;Write a function which returns a map containing the number of occurences of each distinct item in a sequence.
	
(def my-count
  (fn f
    ([list] (f list {}))
    ([list map]
      (if (empty? list)
        map
        (recur (rest list) (update-in map [(first list)] (fnil inc 0)))))))

(unit-test 
  "problem55"
  (= (my-count [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
  (= (my-count [:b :a :b :a :b]) {:a 2, :b 3})
  (= (my-count '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))

;;#46
;;Write a higher-order function which flips the order of the arguments of an input function.

(def my-flip-args
  (fn [func]
    (fn [& args]
      (apply func (reverse args)))))

(unit-test 
  "problem46"
  (= 3 ((my-flip-args nth) 2 [1 2 3 4 5]))
  (= true ((my-flip-args >) 7 8))
  (= 4 ((my-flip-args quot) 2 8))
  (= [1 2 3] ((my-flip-args take) [1 2 3 4 5] 3)))

;;83
;; Write a function which takes a variable number of booleans. Your function should return true if some of the parameters are true, but not all of the parameters are true. Otherwise your function should return false.

(def my-true-checker
  (fn [& args]
    (if (reduce
          (fn [r e] (or r e))
          false
          args)
      (not (every? true? args))
      false
      )))

(unit-test 
  "problem83"
  (= false (my-true-checker false false))	
  (= true (my-true-checker true false))
  (= false (my-true-checker true))
  (= true (my-true-checker false true false))
  (= false (my-true-checker true true true))
  (= true (my-true-checker true true true false)))


;;43
;;Write a function which reverses the interleave process into x number of subsequences.

(def my-rev-interleave 
  (fn [l n]
    (reduce 
      (fn [[h & t] e] (concat t [(conj h e)])) 
      (for [x (range n)] []) 
      l)
    ))

(unit-test 
  "problem43"
  (= (my-rev-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
  (= (my-rev-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
  (= (my-rev-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))

;;50
;;Write a function which takes a sequence consisting of items with different types and splits them up into a set of homogeneous sub-sequences. The internal order of each sub-sequence should be maintained, but the sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases).

(def my-split-type 
  #(vals (group-by class %)))

(unit-test
  "problem50"
  (= (set (my-split-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
  (= (set (my-split-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
  (= (set (my-split-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))

;;56
;;Write a function which removes the duplicates from a sequence. Order of the items must be maintained.

(def my-remove-dup
  #(reduce (fn [t e] (if (some (partial = e) t) t (conj t e))) [] %)
  )

(unit-test
  "problem56"
  (= (my-remove-dup [1 2 1 3 1 2 4]) [1 2 3 4])
  (= (my-remove-dup [:a :a :b :b :c :c]) [:a :b :c])
  (= (my-remove-dup '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
  (= (my-remove-dup (range 50)) (range 50)))

;;58
;;Write a function which allows you to create function compositions. The parameter list should take a variable number of functions, and create a function applies them from right-to-left.

(def my-comp
  (fn [& args]
    (reduce (fn [tot e] #(tot (apply e %&))) identity args)
    ))

(unit-test
  "problem58"
  (= [3 2 1] ((my-comp rest reverse) [1 2 3 4]))
  (= 5 ((my-comp (partial + 3) second) [1 2 3 4]))
  (= true ((my-comp zero? #(mod % 8) +) 3 5 7 9))
  (= "HELLO" ((my-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world")))
