(ns ambiparse.manual-test
  (:require [fipp.edn :refer [pprint]]
            [ambiparse :as a]
            [ambiparse.gll :as gll]
            [ambiparse-test :refer :all]))

(defn party [pat s]
  (pprint (a/parses pat s {:fuel 100 :viz true})
          {:width 160}))

(comment

  (party \x "x")
  (party \y "x")
  (party "xy" "xy")
  (party "xy" "xz")
  (party (a/lit :x) [:x])
  (party (a/lit :x) [:y])
  (party (a/pred pos?) [5])
  (party (a/pred pos?) [-2])
  (party (a/cat) "")
  (party (a/cat) "x")
  (party (a/cat \x) "x")
  (party (a/cat \x \y) "x")
  (party (a/cat \x \y) "xy")
  (party (a/cat \x \y) "xz")
  (party (a/cat \x) "xy")
  (party (a/cat \x \y \z) "xy")
  (party (a/cat \x \y \z) "xyz")
  (party (a/alt) "")
  (party (a/alt \x) "x")
  (party (a/alt \x \y) "x")
  (party (a/alt \x \y) "y")
  (party (a/alt \x \y) "z")
  (party (a/alt (a/cat \x \y) \z) "xo")
  (party (a/cat (a/alt \a (a/cat (a/alt \b \x)))) "x")
  (party (a/cat \a (a/alt \x \y) \b \b) "axbb")
  (party (a/cat \a (a/? (a/cat \b \c)) \d) "abd")
  (party (a/* \x) "")
  (party (a/* \x) "x")
  (party (a/* \x) "xx")
  (party (a/* \x) "xxy")
  (party (a/cat (a/* \x) \y) "xxy")
  (party (a/+ \x) "")
  (party (a/+ \x) "x")
  (party (a/+ \x) "xx")
  (party (a/+ \x) "xxy")
  (party (a/cat (a/+ \x) \y) "xxy")
  (party (a/? \x) "")
  (party (a/? \x) "x")
  (party (a/? \x) "y")
  (party (a/cat (a/? \x) \z) "xz")
  (party (a/cat (a/? \x) \z) "yz")
  (party (a/cat (a/? \x) \y) "yz")
  (party (a/rule \x 1) "x")
  (party (a/rule \x [%]) "x")
  (party (a/rule \x (/ 1 0)) "x")
  (party (a/label :lbl \x) "x")
  (party (a/label :a (a/label :b \x)) "x")
  (party (a/prefer (constantly 0) \x) "x")
  (party (a/cat (a/* \x) (a/? \x)) "xx")
  (party (a/cat (a/greedy (a/* \x)) (a/? \x)) "xxxxx")

  (party a/eof "")
  (party (a/cat \x a/eof) "x")

  (party XS "xxxx")

  (party \x "y")
  (party (a/cat \x \y) "zy")
  (party (a/cat \x \y) "xz")
  (party (a/* \x) "xxxxx")
  (party (a/prefer (fn [t u] (throw (Exception. "whoops")))
                   (a/cat (a/cat \x \x) \x)
                   (a/cat \x \x (a/cat \x)))
         "xxx")

  (party (a/filter (constantly true) \x) "x")
  (party (a/filter (constantly false) \x) "x")
  (party (a/remove (constantly true) \x) "x")
  (party (a/remove (constantly false) \x) "x")

  (party A "a")

  (party B "bbb")
  (party (a/right (a/cat B B)) "bbb")
  (party (a/left (a/cat B B)) "bbb")

  (party #'C "")

  (party #'D "dd")

  (party #'E "ee")

  (party L "xxx")
  (party R "xxx")

  (party T "")
  (party U "")
  (party V "")
  (party W "")

  (party (a/rule \x (a/fail! "reject")) "x")
  (party (a/rule \x (a/fail! "reject")) "y")

  (party (a/dispatch (a/alt \a \b)
           (case (::a/value %)
             \a \x
             \b \y))
         "ax")

  (party (a/cat \x (a/unambiguous (a/pred pos?) (a/pred even?)) \y)
         [\x 5 \y])
  (party (a/cat \x (a/unambiguous (a/pred pos?) (a/pred even?)) \y)
         [\x -1 \y])
  (party (a/cat \x (a/unambiguous (a/pred pos?) (a/pred even?)) \y)
         [\x 4 \y])

  (binding [gll/breaks [0 3 7]]
    (doseq [i (range 9)]
      (prn i (gll/pos-at i))))

)
