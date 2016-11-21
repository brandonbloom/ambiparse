(ns ambiparse.calc-test
  (:require [clojure.test :refer :all]
            [ambiparse :as a]
            [ambiparse.util :refer :all]))

(def Digit
  (a/rule (apply a/alt (map #(char (+ (int \0) %)) (range 10)))
          (- (-> % ::a/value int) (int \0))))

(def Num
  (a/rule (a/+ Digit)
          (reduce (fn [n d]
                    (+ (* n 10) d))
                  0 (::a/value %))))

(def Space
  (a/+ \space))

(declare Expr)

(defn flat-op [op f]
  (a/flat (a/rule (a/cat (a/label :first #'Expr)
                         (a/label :rest (a/* (a/cat Space #'Expr))))
                  (apply f (:first %) (->> % :rest (map second))))))

(defn binop [op f]
  (a/rule (a/cat (a/label :lhs #'Expr) Space
                 (a/label :op op) Space
                 (a/label :rhs #'Expr))
          (f (:lhs %) (:rhs %))))

;;XXX Use flat-op for these after fixing infinite-loop / stack-overflow.
(def Plus (binop \+ +))
(def Times (binop \* *))

(def Minus (a/left (binop \- -)))
(def Divide (a/left (binop \/ /)))
(def Pow (a/right (binop \^ #(Math/pow %1 %2))))

(def priority
  {\^ 1
   \* 2
   \/ 2
   \+ 3
   \- 3})

(def Binops
  (a/prefer (fn [x y] (compare-key (comp priority :op) x y))
            Plus Minus Times Divide Pow))

(def Expr
  (a/alt Num Binops))

(deftest calc-test
  (are [s n] (== (a/parse! Expr s) n)
    "5" 5
    "15" 15
    "2 + 3" 5
    "2 + 3 + 1" 6
    "2 ^ 3" 8
    "2 ^ 3 ^ 2" 512
    "2 * 4 + 3" 11
    "1 / 2 / 2" 0.25
    ))

(comment
  (a/parse! Expr "2 + 4 + 3")
)
