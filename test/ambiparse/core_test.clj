(ns ambiparse.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ambiparse.core :as a :refer [defprod]]))


(declare yx-list)

(defprod xy-list #{[\x #'yx-list]
                   #{\x []}})

(defprod yx-list #{[\y xy-list]
                   \y})

(defprod a-list #{[#'a-list \a] \a})

(defprod a-list-a #{[a-list \a] ""})



(deftest expand-test
  (are [x y] (= (a/expand x) y)

    []            (a/cat)
    #{}           (a/alt)
    \x            (a/lit \x)
    "abc"         (a/cat (a/lit \a) (a/lit \b) (a/lit \c))
    #{\a "xy"}    (a/alt (a/lit \a)
                         (a/cat (a/lit \x) (a/lit \y)))
    [(a/lit \x)]  (a/cat (a/lit \x))

    #'yx-list     (a/prod #'yx-list)
    yx-list       (a/prod #'yx-list)

    ))

(defn nullable? [lang]
  (a/with-cache
    (-> lang a/expand a/nullable?)))

(defn null? [lang]
  (a/with-cache
    (-> lang a/expand a/null?)))

(defn none? [lang]
  (a/with-cache
    (-> lang a/expand a/none?)))

(deftest nullable?-test
  (are [lang bool] (= (nullable? lang) bool)

    \x      false
    ""      true
    #{}     false
    [\x \y] false

    xy-list  true
    yx-list  false
    a-list   false
    a-list-a true

    ))

(deftest null?-test
  (are [lang bool] (= (null? lang) bool)

    \x false
    [] true
    [[]] true
    [[] \x] false
    #{} false
    #{\x []} false

    ))

(deftest none?-test
  (are [lang bool] (= (none? lang) bool)

    \x false
    [] false
    [[]] false
    [[] \x] false
    #{} true
    #{#{}} true
    #{#{} \x} false

    ))

(deftest simplify-test
  (are [x y] (= (-> x a/expand a/simplify) (a/expand y))

    \x                     \x
    []                     []
    [\x]                   \x
    ["abc"]                "abc"
    [""]                   []
    ["x"]                  \x
    [[] \x []]             \x
    [["x"]]                \x
    ["abc" "xyz"]          "abcxyz"
    [#{}]                  #{}
    #{[]}                  []
    #{}                    #{}
    #{\x}                  \x
    #{#{\x \y} #{\y \z}}   #{\x \y \z}
    #{#{#{\x}}}            \x
    #{\x ""}               #{\x []}

    ))

(deftest parse-test
  (are [lang in out] (= (a/parse lang in) out)

    "a"       "b"        #{}
    "a"       "aXc"      #{}

    "a"       "a"        #{\a}
    "abc"     "abc"      #{[\a \b \c]}
    #{\a \b}  "a"        #{\a}

    xy-list   "xyxy"     #{[::xy-list
                            [\x [::yx-list
                                 [\y [::xy-list
                                      [\x [::yx-list \y]]]]]]]
                           [::xy-list
                            [\x [::yx-list
                                 [\y [::xy-list
                                      [\x [::yx-list
                                           [\y [::xy-list []]]]]]]]]]}

    ))

(comment

  (a/parse "abc" "abc")

  (require 'fipp.edn)
  (a/with-cache
    (-> xy-list
        (a/parse "xyxy")
        ;a/expand
        ;(a/derive \x)
        ;a/simplify
        ;a/parses
        fipp.edn/pprint
        ))

  (a/parses (a/alt (a/lit \x)))

)

