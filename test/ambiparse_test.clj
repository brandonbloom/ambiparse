(ns ambiparse-test
  (:require [clojure.test :refer :all]
            [ambiparse :as a]
            [ambiparse.util :refer :all]))

;; Non-recursive var.
(def XS (a/+ \x))

;; Simple left and right recursion.
(def L (a/cat (a/? #'L) \x))
(def R (a/cat \x (a/? #'R)))

;; Various other infinitely recursive grammars.
(def A (a/alt \a #'A))
(def B (a/alt \b (a/cat #'B #'B)))
(def C (a/cat #'C))
(def D (a/alt (a/cat #'D #'D) \d))

;; Infinite zero-width matches.
(def T (a/* (a/cat)))
(def U (a/+ (a/cat)))
(def V (a/cat (a/? #'V) a/eof))

(deftest parses-test
  (are [pat s ts] (= (set (a/parses pat s {:fuel 500})) ts)

    \x "x" #{\x}
    \y "x" #{}

    "" "" #{""}
    "" "x" #{}
    "xy" "xy" #{"xy"}
    "xy" "xz" #{}

    (a/lit :x) [:x] #{:x}

    (a/pred even?) [1] #{}
    (a/pred even?) [2] #{2}

    (a/cat) "" #{[]}
    (a/cat) "x" #{}
    (a/cat \x) "x" #{[\x]}
    (a/cat \x) "xy" #{}
    (a/cat \x \y) "x" #{}
    (a/cat \x \y) "xy" #{[\x \y]}
    (a/cat \x \y \z) "xyz" #{[\x \y \z]}

    a/eof "" #{::a/eof}
    (a/cat \x a/eof) "x" #{[\x ::a/eof]}

    (a/alt \x \y) "x" #{\x}
    (a/alt \x \y) "y" #{\y}
    (a/alt \x \y) "z" #{}

    (a/* \x) "" #{[]}
    (a/* \x) "x" #{[\x]}
    (a/* \x) "xx" #{[\x \x]}

    (a/+ \x) "" #{}
    (a/+ \x) "x" #{[\x]}
    (a/+ \x) "xx" #{[\x \x]}

    (a/? \x) "" #{nil}
    (a/? \x) "x" #{\x}

    (a/cat (a/* \x) (a/? \x)) "xxx" #{[[\x \x \x] nil] [[\x \x] \x]}

    #'XS "xx" #{[\x \x]}

    L "xxx" #{[[[nil \x] \x] \x]}
    R "xxx" #{[\x [\x [\x nil]]]}

    #'A "a" #{\a}
    #'B "bb" #{[\b \b]}
    #'C "c" #{}
    #'D "dd" #{[\d \d]}

    T "" #{[] [[]]}
    U "" #{[[]]}

    (a/prefer (constantly 0) \x) "x" #{\x}
    (a/cat (a/greedy (a/* \x)) (a/? \x)) "xxx" #{[[\x \x \x] nil]}
    ;;XXX prefer test cycle

    (a/cat (a/filter #(>= (a/length %) 2)
                     (a/* \x))
           (a/? \x))
    "xxx"
    #{[[\x \x] \x] [[\x \x \x] nil]}

    (a/cat (a/remove #(< (a/length %) 2)
                     (a/* \x))
           (a/? \x))
    "xxx"
    #{[[\x \x] \x] [[\x \x \x] nil]}

    (a/filter #(= (::a/value %) \x) \x) "x" #{\x}

    ))

(defn clean-error [[t err]]
  [t (cond-> err
       (contains? err ::a/exception) (update ::a/exception
                                             #(.getMessage ^Exception %))
       (contains? err ::a/predicate) (assoc ::a/predicate '...)
       (contains? err ::a/candidates) (assoc ::a/candidates '#{...}))])

(deftest errors-test
  (are [pat s err] (= (clean-error (a/parse pat s)) [nil err])

    ;; Unexpected terminal.
    \x "y"
    {::a/expected \x ::a/actual \y
     ::a/pos {:idx 0 :line 1 :col 1}}

    ;; Unexpected end of input.
    \x ""
    {::a/expected \x ::a/actual ::a/eof
     ::a/pos {:idx 0 :line 1 :col 1}}

    ;; Unexpected string.
    "abcd" "abxd"
    {::a/expected "abcd" ::a/actual "abxd"
     ::a/pos {:idx 2 :line 1 :col 3}}

    ;; Short string.
    "xy" "x"
    {::a/expected "xy" ::a/actual "x"
     ::a/pos {:idx 1 :line 1 :col 2}}

    ;; Lit failure in non-string input.
    (a/lit :x) [:y]
    {::a/expected :x ::a/actual :y
     ::a/pos {:idx 0}}

    ;; Predicate failed.
    (a/pred even?) [1]
    {::a/message "Predicate failed"
     ::a/predicate '...
     ::a/expression 'even?
     ::a/actual 1
     ::a/pos {:idx 0}}

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
    {::a/pos {:idx 0 :line 1 :col 1}
     ::a/alts #{{::a/expected \x ::a/actual \z
                 ::a/pos {:idx 0 :line 1 :col 1}}
                {::a/expected \y ::a/actual \z
                 ::a/pos {:idx 0 :line 1 :col 1}}}}

    ;; Tail zero-or-more failure.
    (a/* (a/cat \x \y)) "x"
    {::a/expected \y ::a/actual ::a/eof
     ::a/pos {:idx 1 :line 1 :col 2}}

    ;; First one-or-more failure.
    (a/+ (a/cat \x \z)) "yz"
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
    {::a/message "Filter predicate failed"
     ::a/predicate '...
     ::a/expression '(constantly false)
     ::a/candidates '#{...}
     ::a/pos {:idx 0 :line 1 :col 1}}

    ;; Remove predicate failed.
    (a/remove (constantly true) \x) "x"
    {::a/message "Filter predicate failed"
     ::a/predicate '...
     ::a/expression '(comp not (constantly true))
     ::a/candidates '#{...}
     ::a/pos {:idx 0 :line 1 :col 1}}

    ))
