(ns ambiparse-test
  (:require [clojure.test :refer :all]
            [ambiparse :as a]
            [ambiparse.util :refer :all]))

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

    (a/prefer (constantly 0) \x) "x" #{\x}
    (a/cat (a/greedy (a/* \x)) (a/? \x)) "xxx" #{[[\x \x \x] []]}
    ;;XXX prefer test cycle

    (a/cat (a/filter #(>= (a/length %) 2)
                     (a/* \x))
           (a/? \x))
    "xxx"
    #{[[\x \x] [\x]] [[\x \x \x] []]}

    (a/cat (a/remove #(< (a/length %) 2)
                     (a/* \x))
           (a/? \x))
    "xxx"
    #{[[\x \x] [\x]] [[\x \x \x] []]}

    (a/filter #(= (::a/value %) \x) \x) "x" #{\x}

    ))

(defn clean-error [[t err]]
  [t (cond-> err
       (contains? err ::a/exception) (update ::a/exception #(.getMessage %))
       (contains? err ::a/predicate) (assoc ::a/predicate '...
                                            ::a/candidates '#{...}))])

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

    ;XXX Check failure of cyclic prefer.

    ;; Filter pattern failed.
    (a/filter (constantly false) \x) "y"
    {::a/expected \x
     ::a/actual \y
     ::a/pos {:idx 0 :line 1 :col 1}}

    ;; Filter predicate failed.
    (a/filter (constantly false) \x) "x"
    {::a/message "Predicate failed"
     ::a/predicate '...
     ::a/expression '(constantly false)
     ::a/candidates '#{...}
     ::a/pos {:idx 0 :line 1 :col 1}}

    ;; Remove predicate failed.
    (a/remove (constantly true) \x) "x"
    {::a/message "Predicate failed"
     ::a/predicate '...
     ::a/expression '(comp not (constantly true))
     ::a/candidates '#{...}
     ::a/pos {:idx 0 :line 1 :col 1}}

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
  (a/rule (a/cat (a/label :lhs #'Expr) Space
                 (a/label :op c) Space
                 (a/label :rhs #'Expr))
          (f (:lhs %) (:rhs %))))

(def Plus (binop \+ +))
(def Minus (binop \- -))
(def Times (a/left (binop \* *)))
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
  (a/parse! Expr "2 * 4 + 3")
)
