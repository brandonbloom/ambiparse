(ns ambiparse
  (:refer-clojure :exclude [cat * + filter remove cons])
  (:require [ambiparse.gll :as gll]
            [ambiparse.util :refer :all]))

(alias 'a 'ambiparse)

;; Reserve the ambiparse namespace for non-label metadata.
(create-ns 'ambiparse.core)
(alias 'c 'ambiparse.core)

;;; Primitives.

(def eof `eof)

(defn lit [x]
  (with-meta (list `lit x)
             {::a/head-fail #(not= % x)}))

(defn -pred [expr f]
  (with-meta (list `-pred expr f)
             {::a/head-fail (comp not f)}))

(defmacro pred [f]
  `(-pred '~f ~f))

(defn cat [& pats]
  (with-meta (list* `cat pats)
             {::a/head-fail (-> pats first meta ::a/head-fail)}))

(defn alt [& pats]
  (let [pat (list* `alt pats)]
    (if-let [head-fail (and (seq pats)
                            (every? (comp ::a/head-fail meta) pats)
                            (apply every-pred (comp ::a/head-fail meta) pats))]
      (with-meta pat {::a/head-fail head-fail})
      pat)))

(defn * [pat]
  (list `* pat))

(defn + [pat]
  (with-meta (list `+ pat)
             {::a/head-fail (-> pat meta ::a/head-fail)}))

(defn ? [pat]
  (list `? pat))

(defn -rule [pat expr f]
  (with-meta (list `-rule pat expr f)
             {::a/head-fail (-> pat meta ::a/head-fail)}))

(defmacro rule [pat & body]
  `(-rule ~pat '~body
          (fn [~'%]
            (assoc ~'% ::value (do ~@body)))))

(defn label [name pat]
  (with-meta (list `label name pat)
             {::a/head-fail (-> pat meta ::a/head-fail)}))

(defn -prefer [expr pat f]
  (with-meta (list `-prefer expr pat f)
             {::a/head-fail (-> pat meta ::a/head-fail)}))

(defmacro prefer [f pat & pats]
  `(-prefer '~f
            ~(if (seq pats)
               `(alt ~pat ~@pats)
               pat)
            ~f))

(defn -filter [expr pat f]
  (with-meta (list `-filter expr pat f)
             {::a/head-fail (-> pat meta ::a/head-fail)}))

(defmacro filter [f pat]
  `(-filter '~f ~pat ~f))

(defn scope
  ([pat]
   (with-meta (list `scope pat)
              {::a/head-fail (-> pat meta ::a/head-fail)}))
  ([pat & pats]
   (scope (apply cat pat pats))))

(defn add! [var pat]
  (assert (var? var))
  (change! gll/muts conj [:add var pat]))

(defn del! [var pat]
  (assert (var? var))
  (change! gll/muts conj [:del var pat]))

(defn fail!
  ([msg] (fail! msg {}))
  ([msg data]
   (throw (ex-info msg (assoc data ::a/failure true)))))


;;; Execution.

(defn parses
  ([pat s] (parses pat s {}))
  ([pat s opts]
   (gll/with-run pat s opts
     (gll/parses))))

(defn parse
  ([pat s] (parse pat s {}))
  ([pat s opts]
   (gll/with-run pat s opts
     (gll/parse))))

(defn parse!
  ([pat s] (parse! pat s {}))
  ([pat s opts]
   (gll/with-run pat s opts
     (gll/parse!))))


;;; Library.

(def digit
  (pred #(and (char? %) (Character/isDigit ^Character %))))

(def alpha
  (pred #(and (char? %) (Character/isLetter ^Character %))))

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

(defn nested? [t]
  (let [s (::structure t)]
    (some #(= (::structure %) s) (::elements t))))

(defn nested-left? [t]
  (nested-at? first t))

(defn nested-right? [t]
  (nested-at? peek t))

(defn left [pat]
  (remove nested-right? pat))

(defn right [pat]
  (remove nested-left? pat))

;;TODO: Test this.
(defn flat [pat]
  (remove nested? pat))

(defn interpose* [sep elem]
  (rule (? (cat (label ::c/first elem) (label ::c/rest (* (cat sep elem)))))
        (if (-> % ::value seq)
          (into [(::c/first %)] (->> % ::c/rest (map second)))
          [])))

(defn cons [x seq]
  (rule (cat (label ::c/first x) (label ::c/rest seq))
        (list* (::c/first %) (::c/rest %))))
