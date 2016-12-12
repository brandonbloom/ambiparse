(ns ambiparse.adaptive-test
  (:require [clojure.test :refer :all]
            [ambiparse :as a]))

(def Atom \a)

(def Add (a/rule (a/cat \+ (a/label :x (a/pred any?)))
                 (a/add! #'Atom (:x %) (:x %))
                 (str "+" (:x %))))

(def Del (a/rule (a/cat \- (a/label :x (a/pred any?)))
                 (a/del! #'Atom (:x %))
                 (str "-" (:x %))))

(declare Commands)

(def Block (a/rule (a/scope (a/cat \[ (a/label :xs #'Commands) \]))
                   (:xs %)))

(def Command (a/alt #'Atom Add Del Block))

(def Commands (a/interpose* \space Command))

(deftest add-del-test
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
  (are [s] (not (seq? (a/parses Commands s)))
    "b"
    "a b"
    "+b c"
    "[+b b] b"
    "+b +c b c -b b"
    ))

(deftest bind-test
  (let [x (a/rule \x
            (a/bind! ::count (inc (or (a/resolve ::count) 0)))
            nil)
        xs (a/rule (a/* x)
             {:env (::a/env %)
              :count (a/resolve ::count)})
        {:keys [env count]} (a/parse! xs "xxxx")
        _ (is (= count 4))
        {:keys [count]} (a/parse! xs "xx" {:env env})
        _ (is (= count 6))]
    ))

(comment

  (defn party [s]
    (fipp.edn/pprint (a/parse! Commands s {:viz true})))

  (party "a +b b")

)
