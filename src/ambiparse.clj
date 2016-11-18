(ns ambiparse
  (:refer-clojure :exclude [cat * +])
  (:require [ambiparse.gll :as gll]))

(defn cat [& pats]
  (list* `cat pats))

(defn alt [& pats]
  (list* `alt pats))

(defn * [pat]
  (list `* pat))

(defn + [pat]
  (list `+ pat))

(defn -rule [pat f]
  (list `-rule pat f))

(defmacro rule [pat & body]
  `(-rule ~pat
          (fn [~'%]
            (assoc ~'% ::value (do ~@body)))))

(defn label [name pat]
  (list `label name pat))

(defn parses [pat s]
  (gll/with-run pat s
    (gll/parses)))

(defn parse [pat s]
  (gll/with-run pat s
    (gll/parse)))

(defn parse! [pat s]
  (gll/with-run pat s
    (gll/parse!)))

(comment

  (require 'fipp.edn)
  (defn party [pat s]
    (gll/with-run pat s
      (fipp.edn/pprint {:parses (gll/parses)
                        ;:generated (gll/generated gll/root)
                        :failure (gll/failure)}
                       {:width 200})))

  (party \x "x")
  (party (cat) "")
  (party (cat) "x")
  (party (cat \x) "x")
  (party (cat \x \y) "x")
  (party (cat \x \y) "xy")
  (party (cat \x \y \z) "xy")
  (party (cat \x \y \z) "xyz")
  (party (alt) "")
  (party (alt \x) "x")
  (party (alt \x \y) "x")
  (party (alt \x \y) "y")
  (party (alt \x \y) "z")
  (party (* \x) "")
  (party (* \x) "x")
  (party (* \x) "xx")
  (party (+ \x) "")
  (party (+ \x) "x")
  (party (+ \x) "xx")
  (party (rule \x [%]) "x")
  (party (label :lbl \x) "x")

  (party \x "y")
  (party (cat \x \y) "zy")
  (party (cat \x \y) "xz")
  (party (* \x) "xxxxx")

)
