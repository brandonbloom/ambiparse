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
  (gll/parses pat s))

(defn parse [pat s]
  (gll/parse pat s))

(comment

  (require 'fipp.edn)
  (fipp.edn/pprint
    (parses \x "x")
    ;(parses (cat) "")
    ;(parses (cat) "x")
    ;(parses (cat \x) "x")
    ;(parses (cat \x \y) "x")
    ;(parses (cat \x \y) "xy")
    ;(parses (cat \x \y \z) "xy")
    ;(parses (cat \x \y \z) "xyz")
    ;(parses (alt) "")
    ;(parses (alt \x) "x")
    ;(parses (alt \x \y) "x")
    ;(parses (alt \x \y) "y")
    ;(parses (alt \x \y) "z")
    ;(parses (* \x) "x")
    ;(parses (* \x) "xx")
    ;(parses (rule \x [%]) "x")
    ;(parses (label :lbl \x) "x")
    )

)
