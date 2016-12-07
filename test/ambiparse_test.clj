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
(def W (a/cat (a/? #'W) (a/cat)))

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
    V "" #{[nil ::a/eof] [[nil ::a/eof] ::a/eof]}
    W "" #{[nil []] [[nil []] []]}

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

(defn clean-error
  "Simplifies errors to remove object identity and minimize test fragility."
  [err]
  (cond-> err
    (contains? err :exception) (update :exception #(.getMessage ^Exception %))
    (contains? err :predicate) (assoc :predicate '...)
    (contains? err :candidates) (update :candidates
                                        #(->> % (map ::a/value) set))))

(defn clean-failure [[t fail]]
  [t (when fail
       (update fail :errors #(->> % (map clean-error) set)))])

(deftest errors-test
  (are [pat s i errs]
       (let [ret (a/parse pat s)
             [t {:keys [pos errors]}] (clean-failure ret)]
         (prn [t (:idx pos) errors])
         (= [t (:idx pos) errors] [nil i errs]))

    ;; Unexpected terminal.
    \x "y"
    0 #{{:expected \x}}

    ;; Unexpected end of input.
    \x ""
    0 #{{:expected \x}}

    ;; Unexpected string.
    "abcd" "abxd"
    2 #{{:expected "abcd" :actual "abxd"}}

    ;; Short string.
    "xy" "x"
    1 #{{:expected "xy" :actual "x"}}

    ;; Lit failure in non-string input.
    (a/lit :x) [:y]
    0 #{{:expected :x}}

    ;; Predicate failed.
    (a/pred even?) [1]
    0 #{{:message "Predicate failed" :predicate '...  :expression 'even?}}

    ;; Failure in element of concatenation.
    (a/cat \x \y) "zy"
    0 #{{:expected \x}}
    (a/cat \x \y) "xz"
    1 #{{:expected \y}}

    ;;XXX (party (a/cat) "x")

    ;; Failure in optional element of concatenation.
    (a/cat \a (a/? (a/cat \b \c)) \d) "abd"
    2 #{{:expected \c}}

    ;;XXX Unexpected eof in cat.

    ;; Rightmost failure from alt.
    (a/alt (a/cat \x \y) \z) "xo"
    1 #{{:expected \y}}

    ;; Multiple rightmost failures.
    (a/alt \x \y) "z"
    0 #{{:expected \x}
        {:expected \y}}

    ;; Nested rightmost failures.
    (a/alt \x (a/alt \y \z)) "w"
    0 #{{:expected \x}
        {:expected \y}
        {:expected \z}}

    ;; Tail zero-or-more failure.
    (a/* (a/cat \x \y)) "x"
    1 #{{:expected \y}}

    ;; First one-or-more failure.
    (a/+ (a/cat \x \z)) "yz"
    0 #{{:expected \x}}

    ;; Rule pattern failure.
    (a/rule \x \z) "y"
    0 #{{:expected \x}}

    ;; Rule expression exception.
    (a/rule \x (/ 1 0)) "x"
    1 #{{:exception "Divide by zero"}}

    ;; Explicit failure rule.
    (a/rule \x (a/fail! "oh noez!" {:x 123})) "x"
    1 #{{:message "oh noez!"
         :data {:x 123}}}

    ;; Label pattern failure.
    (a/label :foo (a/+ \x)) "y"
    0 #{{:expected \x}}

    ;; Var pattern failure.
    #'XS "y"
    0 #{{:expected \x}} ;XXX include var.

    ;; Prefer compare failure.
    (a/prefer (fn [t u] (throw (Exception. "whoops")))
              (a/cat (a/cat \x \x) \x)
              (a/cat \x \x (a/cat \x)))
    "xxx"
    3 #{{:exception "whoops"}}

    ;; Prefer pattern failure.
    (a/greedy (a/+ \x)) "y"
    0 #{{:expected \x}}

    ;XXX Check failure of cyclic prefer.

    ;; Filter pattern failed.
    (a/filter (constantly false) \x) "y"
    0 #{{:expected \x}}

    ;; Filter predicate failed.
    (a/filter (constantly false) \x) "x"
    0 #{{:message "Filter predicate failed"
         :predicate '...
         :expression '(constantly false)
         :candidates #{\x}}}

    ;; Remove predicate failed.
    (a/remove (constantly true) \x) "x"
    0 #{{:message "Filter predicate failed"
         :predicate '...
         :expression '(comp not (constantly true))
         :candidates #{\x}}}

    ))
