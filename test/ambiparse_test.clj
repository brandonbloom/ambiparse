(ns ambiparse-test
  (:require [clojure.test :refer :all]
            [ambiparse :as a]))

(def XS (a/+ \x))

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
    (a/? \x) "" #{[]}
    (a/? \x) "x" #{[\x]}
    (a/cat (a/* \x) (a/? \x)) "xxx" #{[[\x \x \x] []] [[\x \x] [\x]]}
    #'XS "xx" #{[\x \x]}
    ))

(deftest prefer-test
  (are [pat s t] (= (a/parse! pat s) t)
    (a/prefer (constantly 0) \x) "x" \x
    (a/cat (a/greedy (a/* \x)) (a/? \x)) "xxx" [[\x \x \x] []]
    ;;XXX test cycle
    ))

(defn clean-error [[t err]]
  (if (contains? err ::a/exception)
    [t (update err ::a/exception #(.getMessage %))]
    [t err]))

(deftest errors-test
  (are [pat s err] (= (clean-error (a/parse pat s)) [nil err])

    ;; Unexpected terminal.
    \x "y"
    {::a/expected \x ::a/actual \y
     ::a/pos {:idx 0 :line 1 :col 1}}

    ;XXX \x "xz" {:UNEXPECTED_EOF '?
    ;XXX          ::a/pos {:idx 1 :line 1 :col 2}}

    ;; Failure in element of concatenation.
    (a/cat \x \y) "zy"
    {::a/expected \x ::a/actual \z
     ::a/pos {:idx 0 :line 1 :col 1}}
    (a/cat \x \y) "xz"
    {::a/expected \y ::a/actual \z
     ::a/pos {:idx 1 :line 1 :col 2}}

    ;; Rightmost failure from alt.
    (a/alt (a/cat \x \y) \z) "xo"
    {::a/expected \y ::a/actual \o
     ::a/pos {:idx 1 :line 1 :col 2}}

    ;; Multiple rightmost failures.
    (a/alt \x \y) "z"
    {::a/alts #{{::a/expected \x ::a/actual \z
                 ::a/pos {:idx 0 :line 1 :col 1}}
                {::a/expected \y ::a/actual \z
                 ::a/pos {:idx 0 :line 1 :col 1}}}}

    ;; Rep failure.
    (a/+ \x) "y"
    {::a/expected \x ::a/actual \y
     ::a/pos {:idx 0 :line 1 :col 1}}

    ;; Rule pattern failure.
    (a/rule \x \z) "y"
    {::a/expected \x ::a/actual \y
     ::a/pos {:idx 0 :line 1 :col 1}}

    ;; Rule expression failure.
    (a/rule \x (/ 1 0)) "x"
    {::a/exception "Divide by zero"
     ::a/pos {:idx 0 :line 1 :col 1}}

    ;; Label pattern failure.
    (a/label :foo (a/+ \x)) "y"
    {::a/expected \x ::a/actual \y
     ::a/pos {:idx 0 :line 1 :col 1}}

    ;; Var pattern failure.
    #'XS "y"
    {::a/expected \x ::a/actual \y
     ::a/pos {:idx 0 :line 1 :col 1}
     ::a/var #'XS}

    ;; Prefer compare failure.
    (a/prefer (fn [t u] (throw (Exception. "whoops")))
              (a/cat (a/cat \x \x) \x)
              (a/cat \x \x (a/cat \x)))
    "xxx"
    {::a/exception "whoops"
     ::a/pos {:idx 0 :line 1 :col 1}}

    ;; Prefer pattern failure.
    (a/greedy (a/+ \x)) "y"
    {::a/expected \x ::a/actual \y
     ::a/pos {:idx 0 :line 1 :col 1}}

    ;XXX Handle cyclic var.

    ))

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

(defn binop [c f]
  (a/rule (a/cat (a/label :lhs #'Expr) Space c Space (a/label :rhs #'Expr))
          (f (:lhs %) (:rhs %))))

(def Sum (binop \+ +))
(def Pow (binop \^ #(Math/pow %1 %2)))

(def Expr
  (a/alt Num
         Sum
         Pow))

(deftest calc-test
  (are [s n] (== (a/parse! Expr s) n)
    "5" 5
    "15" 15
    "2 + 3" 5
    "2 + 3 + 1" 6
    "2 ^ 3" 8
    ;TODO: "2 ^ 3 ^ 2" 512
    ))
