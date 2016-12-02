(ns ambiparse.manual-test
  (:require [fipp.edn :refer [pprint]]
            [ambiparse :as a]
            [ambiparse.gll :as gll]))

(defn party [pat s]
  (gll/with-run pat s
    (pprint {:trees (gll/trees)
             :parses (gll/parses)
             :failure (gll/failure)}
            {:width 160})))

(comment

  (party \x "x")
  (party "xy" "xy")
  (party (a/lit :x) [:x])
  (party (a/lit :x) [:y])
  (party (a/cat) "")
  (party (a/cat) "x")
  (party (a/cat \x) "x")
  (party (a/cat \x \y) "x")
  (party (a/cat \x \y) "xy")
  (party (a/cat \x \y) "xz")
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
  (party (a/* \x) "")
  (party (a/* \x) "x")
  (party (a/* \x) "xx")
  (party (a/+ \x) "")
  (party (a/+ \x) "x")
  (party (a/+ \x) "xx")
  (party (a/? \x) "")
  (party (a/? \x) "x")
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

  (def A (a/alt \a (a/cat \a #'A)))
  (party A "aaaa")

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

  (def X (a/alt \x #'X))
  (party X "x")

  (def X (a/alt \x (a/cat) (a/cat #'X #'X)))
  (party X "xxx") ;XXX infinite loop on epsilon!
  (party (a/right (a/cat X X)) "xxx")
  (party (a/left (a/cat X X)) "xxx")

  (def Y (a/cat #'Y))
  (party #'Y "")

  (def Z (a/alt (a/cat #'Z #'Z) \z))
  (party #'Z "zz")

  (binding [gll/breaks [0 3 7]]
    (doseq [i (range 9)]
      (prn i (gll/pos-at i))))

)
