(ns ambiparse
  (:refer-clojure :exclude [cat * + filter remove])
  (:require [ambiparse.gll :as gll]
            [ambiparse.util :refer :all]))

;;; Primitives.

(defn cat [& pats]
  (list* `cat pats))

(defn alt [& pats]
  (list* `alt pats))

(defn * [pat]
  (list `* pat))

(defn + [pat]
  (list `+ pat))

(defn ? [pat]
  (list `? pat))

(defn -rule [pat f]
  (list `-rule pat f))

(defmacro rule [pat & body]
  `(-rule ~pat
          (fn [~'%]
            (assoc ~'% ::value (do ~@body)))))

(defn label [name pat]
  (list `label name pat))

(defn prefer [cmp pat & pats]
  (list `prefer cmp
        (if (seq pats)
          (apply alt pat pats)
          pat)))

(defn -filter [f expr pat]
  (list `filter f expr pat))

(defmacro filter [f pat]
  `(-filter ~f '~f ~pat))


;;; Execution.

(defn parses [pat s]
  (gll/with-run pat s
    (gll/parses)))

(defn parse [pat s]
  (gll/with-run pat s
    (gll/parse)))

(defn parse! [pat s]
  (gll/with-run pat s
    (gll/parse!)))


;;; Library.

(defn length [t]
  (- (-> t ::end :idx) (-> t ::begin :idx)))

(defn longest [pat]
  (prefer (comparator-key length) pat))

(defn greedy [pat]
  (prefer (comparator-key count) pat))

(defmacro remove [f pat]
  `(-filter (comp not ~f) '~(list 'comp 'not f) ~pat))

(defn nested-at? [f t]
  (= (-> t ::elements f ::structure) (::structure t)))

(defn nested-left? [t]
  (nested-at? first t))

(defn nested-right? [t]
  (nested-at? peek t))

(defn left [pat]
  (remove nested-right? pat))

(defn right [pat]
  (remove nested-left? pat))

(defn flat [pat]
  (remove nested? pat))


(comment

  (require 'fipp.edn)
  (alias 'a 'ambiparse)
  (defn party [pat s]
    (gll/with-run pat s
      (fipp.edn/pprint {:trees (gll/trees)
                        :parses (gll/parses)
                        :failure (gll/failure)}
                       {:width 160})))

  (party \x "x")
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

  (def X (a/alt \x (a/cat) (a/cat #'X #'X)))
  (party X "xxx") ;XXX infinite loop on epsilon!
  (party (a/right (a/cat X X)) "xxx")
  (party (a/left (a/cat X X)) "xxx")

)
