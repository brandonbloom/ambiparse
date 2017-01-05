(ns ambiparse
  (:refer-clojure :exclude [cat * + filter remove cons resolve])
  (:require [ambiparse.gll :as gll]
            [ambiparse.util :refer :all]))

;;XXX Avoid single-segment namespace due to JVM "unnamed package" restrictions.

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

(defn -rule [pat body f]
  (with-meta (list `-rule pat body f)
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

;;XXX unambiguous node - buffer at most one parse

(defn scope
  ([pat]
   (with-meta (list `scope pat)
              {::a/head-fail (-> pat meta ::a/head-fail)}))
  ([pat & pats]
   (scope (apply cat pat pats))))

(defn -dispatch [pat body f]
  (with-meta (list `-dispatch pat body f)
             {::a/head-fail (-> pat meta ::a/head-fail)}))

(defmacro dispatch [pat & body]
  `(-dispatch ~pat '~body (fn [~'%] ~@body)))

(defn resolve
  "Retreives the bound value of a qualified keyword in the current parsing
  environment, or nil."
  [id]
  (assert (qualified-keyword? id))
  (some-> (gll/env id) first val))

(defmacro bind!
  "Changes the parsing environment to bind the qualified keyword id to resolve
  to val. Any prior binding for id is removed.  If val has unstable value
  identity (such as a freshly allocated lexical closure), a stable key can
  be supplied to prevent the parser from going in to an infinite loop.
  If the current binding has the same key, body is not evaluated and the
  binding environment remains unchanged."
  ([id val]
   `(let [id# ~id
          val# ~val]
      (bind! id# val# val#)))
  ([id key & body]
   `(let [id# ~id
          key# ~key
          binding# (first (gll/env id#))]
      (assert (qualified-keyword? ~id))
      (when (or (nil? binding#) (not= (key binding#) key#))
        (change! gll/env assoc id# {key# (do ~@body)}))
      nil)))

(defmacro add!
  "Extends the parsing environment with an additional alt pattern for var.
  Replaces existing extensions with the same var and key.  See bind! for a
  warning about environment binding keys."
  [var key & body]
  `(let [v# ~var]
     (assert (var? v#))
     (change! gll/env update-in [v# ~key] #(doto (or % (do ~@body)) assert))
     nil))

(defn del! [var key]
  (assert (var? var))
  (change! gll/env update var dissoc key)
  nil)

(defmacro extend-env
  "Executes body in the given parsing environment, returning the changed env."
  [env & body]
  `(binding [gll/env ~env]
     ~@body
     gll/env))

(defmacro build-env
  "Like extend-env with an empty environment."
  [& body]
  `(extend-env {} ~@body))

(defn fail!
  ([msg] (fail! msg {}))
  ([msg data]
   (throw (ex-info msg (assoc data ::a/failure true)))))


;;; Execution.

(defn parses
  "Returns a lazy-seq of successful parses or a failure.

  Options:
  :unique Produces an ambiguity error if there are multiple successful parses.
  :fuel   Limits the number of steps to perform before giving up.
  :viz    Set to true to pop open a debug visualization of the parser network.
  :env    Initial environment."
  ([pat s] (parses pat s {}))
  ([pat s opts]
   (gll/run pat s opts)))

(defn parse
  "Returns a pair of an unambiguous parse and a failure.
  Only one will be non-nil. See parses."
  ([pat s] (parse pat s {}))
  ([pat s opts]
   (let [ps (parses pat s (assoc opts :unique true))]
     (if (seq? ps)
       [(first ps) nil]
       [nil ps]))))

(defn parse!
  "Returns the success result of parse or throws the failure."
  ([pat s] (parse! pat s {}))
  ([pat s opts]
   (let [[p err] (parse pat s opts)]
     (if err
       (throw (ex-info "Parse failed" err))
       p))))


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
