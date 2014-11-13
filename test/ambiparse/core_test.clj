(ns ambiparse.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ambiparse.core :as lang :refer [cat alt lit prod defprod]]))


(declare yx-list)

(defprod xy-list #{[\x #'yx-list]
                   #{\x []}})

(defprod yx-list #{[\y xy-list]
                   \y})

(defprod a-list #{[#'a-list \a] \a})

(defprod a-list-a #{[a-list \a] ""})



(deftest expand-test
  (are [x y] (= (lang/expand x) y)

    []          (cat)
    #{}         (alt)
    \x          (lit \x)
    "abc"       (cat (lit \a) (lit \b) (lit \c))
    #{\a "xy"}  (alt (lit \a) (cat (lit \x) (lit \y)))
    [(lit \x)]  (cat (lit \x))

    #'yx-list   (prod #'yx-list)
    yx-list     (prod #'yx-list)

    ))

(defn nullable? [lang]
  (lang/with-cache
    (-> lang lang/expand lang/nullable?)))

(defn null? [lang]
  (lang/with-cache
    (-> lang lang/expand lang/null?)))

(defn none? [lang]
  (lang/with-cache
    (-> lang lang/expand lang/none?)))

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
  (are [x y] (= (-> x lang/expand lang/simplify) (lang/expand y))

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
  (are [lang in out] (= (lang/parse lang in) out)

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

  (lang/parse "abc" "abc")

  (lang/with-cache
    (-> xy-list
        (lang/parse "xyxy")
        ;lang/expand
        ;(lang/derive \x)
        ;lang/simplify
        ;lang/parses
        fipp.edn/pprint
        ))

  (lang/parses (lang/alt (lang/lit \x)))

)

