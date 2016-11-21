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

(defn -rule [pat expr f]
  (list `-rule pat expr f))

(defmacro rule [pat & body]
  `(-rule ~pat '~body
          (fn [~'%]
            (assoc ~'% ::value (do ~@body)))))

(defn label [name pat]
  (list `label name pat))

(defn -prefer [expr pat f]
  (list `-prefer expr pat f))

(defmacro prefer [f pat & pats]
  `(-prefer '~f
            ~(if (seq pats)
               `(alt ~pat ~@pats)
               pat)
            ~f))

(defn -filter [expr pat f]
  (list `-filter expr pat f))

(defmacro filter [f pat]
  `(-filter '~f ~pat ~f))


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
  `(-filter '~(list 'comp 'not f) ~pat (comp not ~f)))

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
