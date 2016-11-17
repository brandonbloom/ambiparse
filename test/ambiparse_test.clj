(ns ambiparse-test
  (:require [clojure.test :refer :all]
            [ambiparse :as a]))

(deftest parses-test
  (are [pat s ts] (= (set (a/parses pat s)) ts)
    \x "x" #{\x}
    \y "x" #{}
    (a/cat) "" #{[]}
    (a/cat) "x" #{}
    (a/cat \x) "x" #{[\x]}
    (a/cat \x) "xy" #{}
    (a/cat \x \y) "x" #{}
    (a/cat \x \y) "xy" #{[\x \y]}
    (a/cat \x \y \z) "xyz" #{[\x \y \z]}
    (a/alt \x \y) "x" #{\x}
    (a/alt \x \y) "y" #{\y}
    (a/alt \x \y) "z" #{}
    (a/* \x) "" #{[]}
    (a/* \x) "x" #{[\x]}
    (a/* \x) "xx" #{[\x \x]}
    (a/+ \x) "" #{}
    (a/+ \x) "x" #{[\x]}
    (a/+ \x) "xx" #{[\x \x]}
    ))

(deftest errors-test
  )

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

(def Sum
  (a/rule (a/cat (a/label :lhs #'Expr) Space \+ Space (a/label :rhs #'Expr))
          (+ (:lhs %) (:rhs %))))

(def Expr
  (a/alt Num
         Sum))

(deftest calc-test
  (are [s n] (= (a/parse Expr s) n)
    "5" 5
    "15" 15
    "2 + 3" 5
    "2 + 3 + 1" 6))
