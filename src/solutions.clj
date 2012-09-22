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
	
(def my-occurences
  (fn f
    ([list] (f list {}))
    ([list map]
      (if (empty? list)
        map
        (recur (rest list) (update-in map [(first list)] (fnil inc 0)))))))

(unit-test 
  "problem55"
  (= (my-occurences [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
  (= (my-occurences [:b :a :b :a :b]) {:a 2, :b 3})
  (= (my-occurences '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))

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

;;59
;;Take a set of functions and return a new function that takes a variable number of arguments and returns a sequence containing the result of applying each function left-to-right to the argument list.

(def my-juxt
  (fn [& funs] (fn [& args] (map #(apply % args) funs)))
  )

(unit-test
  "problem59"
  (= [21 6 1] ((my-juxt + max min) 2 3 5 1 6 4))
  (= ["HELLO" 5] ((my-juxt #(.toUpperCase %) count) "hello"))
  (= [2 6 4] ((my-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))

;;54
;;Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.

(def my-partition
  (fn fun [x l]
    (if (< (count l) x)
      '()
      (let [[a & as] (split-at x l)]
        (cons a (fun x (first as))))))
  )

(unit-test
  "problem54"
  (= (my-partition 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
  (= (my-partition 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
  (= (my-partition 3 (range 8)) '((0 1 2) (3 4 5))))

;;21
;;Write a function which returns the Nth element from a sequence.

(def my-nth
  #(first (drop %2 %)))

(unit-test
  "problem21"
  (= (my-nth '(4 5 6 7) 2) 6)
  (= (my-nth [:a :b :c] 0) :a)
  (= (my-nth [1 2 3 4] 1) 2)
  (= (my-nth '([1 2] [3 4] [5 6]) 2) [5 6]))

;;29
;;Write a function which takes a string and returns a new string containing only the capital letters.

(def my-get-capital
  (fn [a] (apply str (filter #(Character/isUpperCase %) (seq a)))))

(unit-test
  "problem29"
  (= (my-get-capital "HeLlO, WoRlD!") "HLOWRD")
  (empty? (my-get-capital "nothing"))
  (= (my-get-capital "$#A(*&987Zf") "AZ"))

;;28
;;Write a function which flattens a sequence.

(def my-flatten
  (fn rec [[x & xs :as l]]
    (cond
      (empty? l) l
      (coll? x) (concat (rec x) (rec xs))
      true (cons x (rec xs))))
  )

(unit-test
  "problem28"
  (= (my-flatten '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
  (= (my-flatten ["a" ["b"] "c"]) '("a" "b" "c"))
  (= (my-flatten '((((:a))))) '(:a)))

;;#70
;;Write a function that splits a sentence up into a sorted list of words. Capitalization should not affect sort order and punctuation should be ignored.

(def my-split-sentence
  (fn [a] 
    (sort 
      String/CASE_INSENSITIVE_ORDER 
       (.split 
         (apply 
           str 
           (filter 
             #(not (= (Character/getType %)
                      Character/OTHER_PUNCTUATION)) 
             (seq a))) 
         " "))))

(unit-test
  "problem70"
  (= (my-split-sentence  "Have a nice day.")
     ["a" "day" "Have" "nice"])
  (= (my-split-sentence  "Clojure is a fun language!")
     ["a" "Clojure" "fun" "is" "language"])
  (= (my-split-sentence  "Fools fall for foolish follies.")
     ["fall" "follies" "foolish" "Fools" "for"]))

;;118
;;Map is one of the core elements of a functional programming language. Given a function f and an input sequence s, return a lazy sequence of (f x) for each element x in s.
(def my-lazy-map
  (fn rec [func l]
    (if (empty? l)
      l
      (cons 
        (func (first l)) 
        (lazy-seq (rec func (rest l)))))))

(unit-test
  "problem118"
  (= [3 4 5 6 7]
     (my-lazy-map inc [2 3 4 5 6]))
  (= (repeat 10 nil)
     (my-lazy-map (fn [_] nil) (range 10)))
  (= [1000000 1000001]
     (->> (my-lazy-map  inc (range))
       (drop (dec 1000000))
       (take 2))))

;;25
;;Write a function which returns only the odd numbers from a sequence.
	
(def my-only-odd
  #(filter odd? %))

(unit-test
  "problem25"
  (= (my-only-odd #{1 2 3 4 5}) '(1 3 5))
  (= (my-only-odd [4 2 1 6]) '(1))
  (= (my-only-odd [2 2 4 6]) '())
  (= (my-only-odd [1 1 1 3]) '(1 1 1 3)))

;;22
;;Write a function which returns the total number of elements in a sequence.

(def my-count
  #(reduce (fn [t _] (inc t)) 0 %))

(unit-test
  "problem22"
  (= (my-count '(1 2 3 3 1)) 5)
  (= (my-count "Hello World") 11)
  (= (my-count [[1 2] [3 4] [5 6]]) 3)
  (= (my-count '(13)) 1)
  (= (my-count '(:a :b :c)) 3))

;;69
;;Write a function which takes a function f and a variable number of maps. Your function should return a map that consists of the rest of the maps conj-ed onto the first. If a key occurs in more than one map, the mapping(s) from the latter (left-to-right) should be combined with the mapping in the result by calling (f val-in-result val-in-latter)

(def my-merge-with
  (fn [fun & maps]
    (reduce
      (fn [outer e]
        (reduce
          (fn [inner [k v]]
            (update-in inner [k] #(if (nil? %) v (fun % v))))
          outer e))
      {} maps)
    )
  )

(unit-test
  "problem69"
  (= (my-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
     {:a 4, :b 6, :c 20})
  (= (my-merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
     {1 7, 2 10, 3 15})
  (= (my-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
     {:a [3 4 5], :b [6 7], :c [8 9]}))

;;23
;;Write a function which reverses a sequence.

(def my-reverse
  (fn rev
    ([l] (rev l (list)))
    ([l acc]
      (if (empty? l)
        acc
        (recur (rest l) (cons (first l) acc)))))
  )

;;Shorter solution -> #(reduce conj '() %)

(unit-test
  "problem23"
  (= (my-reverse [1 2 3 4 5]) [5 4 3 2 1])
  (= (my-reverse (sorted-set 5 7 2 7)) '(7 5 2))
  (= (my-reverse [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]]))

;;27
;;Write a function which returns true if the given sequence is a palindrome.
;;Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)

(def my-is-palindrome?
  #(= (reverse %) (seq %)))

(unit-test
  "problem27"
  (false? (my-is-palindrome? '(1 2 3 4 5)))
  (true? (my-is-palindrome? "racecar"))
  (true? (my-is-palindrome? [:foo :bar :foo]))
  (true? (my-is-palindrome? '(1 1 3 3 1 1)))
  (false? (my-is-palindrome? '(:a :b :c))))

;;143
;;Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same length.

(def my-dot-product
  #(apply + (map * % %2))
  )

(unit-test
  "problem143"
  (= 0 (my-dot-product [0 1 0] [1 0 0]))
  (= 3 (my-dot-product [1 1 1] [1 1 1]))
  (= 32 (my-dot-product [1 2 3] [4 5 6]))
  (= 256 (my-dot-product [2 5 6] [100 10 1])))
