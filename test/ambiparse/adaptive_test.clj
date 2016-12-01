(ns ambiparse.adaptive-test
  (:require [clojure.test :refer :all]
            [ambiparse :as a]))

(def Atom \a)

(def Add (a/rule (a/cat \+ (a/label :x (a/pred any?)))
                 (a/add! #'Atom (:x %))
                 (str "+" (:x %))))

(def Del (a/rule (a/cat \- (a/label :x (a/pred any?)))
                 (a/del! #'Atom (:x %))
                 (str "-" (:x %))))

(declare Commands)

(def Block (a/rule (a/scope (a/cat \[ (a/label :xs #'Commands) \]))
                   (:xs %)))

(def Command (a/alt #'Atom Add Del Block))

(def Commands (a/interpose \space Command))

(deftest adaptive-test
  (are [s t] (= (a/parse! Commands s) t)
    "" []
    "a" [\a]
    "a a" [\a \a]
    "+b b" ["+b" \b]
    "a +b b" [\a "+b" \b]
    "[] a" [[] \a]
    "[+b b]" [["+b" \b]]
    "[+b b] a" [["+b" \b] \a]
    "+b +c c" ["+b" "+c" \c]
    "+b +c b c" ["+b" "+c" \b \c]
    "+b +c b c -b c" ["+b" "+c" \b \c "-b" \c]
    )
  ;;TODO: Check specific failures.
  (are [s] (empty? (a/parses Commands s))
    "b"
    "a b"
    "+b c"
    "[+b b] b"
    "+b +c b c -b b"
    ))

(comment

  (defn party [s]
    (fipp.edn/pprint (a/parse! Commands s)))

  (party "a +b b")

)
