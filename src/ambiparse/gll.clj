(ns ambiparse.gll
  (:refer-clojure :exclude [send])
  (:require [ambiparse.util :refer :all]))

(create-ns 'ambiparse)
(alias 'a 'ambiparse)

;;; Glossary:
;;;   i = index in to source string
;;;   b = begin index of span
;;;   e = end index of span
;;;   pat = pattern
;;;   k = key of node (pair of i and pat)
;;;   t = tree
;;;   c = char (or other atomic terminal)
;;;   s = string (ie. sequence of terminals)
;;;   src = source key of edge
;;;   dst = destination key of edge
;;;   d = decorator attached to edges

;;; Essential state.
(def ^:dynamic input)
(def ^:dynamic graph)
(def ^:dynamic queue)

;;; Debug state.
;(def trace true)
(def trace false)
(def ^:dynamic fuel 0) ; steps to perform before giving up; 0 = disable.

(defmacro log [& xs]
  (require 'fipp.edn)
  (when trace
    `(fipp.edn/pprint (list ~@xs))))

(defn state []
  {:input input
   :graph graph
   :queue queue
   :fuel fuel})

(defn classify [x]
  (cond
    (sequential? x) (first x)
    (keyword? x) x
    :else (class x)))

(defn dispatch [[i x]]
  (classify x))

(doseq [sym '[init decorate tell]]
  (ns-unmap *ns* sym))
(defmulti init dispatch)
(defmulti decorate (fn [t by] (classify by)))
(defmulti tell (fn [k t] (dispatch k)))

(def conjs (fnil conj #{}))

(defn send [msg]
  (log 'send msg)
  (update! queue conj msg)
  nil)

(defn add-node [i pat]
  (let [k [i pat]]
    (when-not (get-in graph k)
      (send [:init k]))
    k))

(defn add-edge [i pat dst d]
  (let [k (add-node i pat)]
    (when-not (get-in graph (conj k :edges dst d))
      (update! graph update-in (conj k :edges dst) conjs d)
      (send [:link k dst d]))
    k))

(defmethod decorate :identity [t _]
  t)

(defmethod decorate :prefix [x [_ xs]]
  (merge xs x {::a/begin (::a/begin xs)
               ::a/end (::a/end x)
               ::a/value (conj (::a/value xs) (::a/value x))}))

(defmethod decorate :single [t _]
  (update t ::a/value vector))

(defn pass [k t]
  (when-not (get-in graph (conj k :parses t))
    (update! graph update-in (conj k :parses) conjs t)
    (doseq [[dst ds] (get-in graph (conj k :edges))
            d ds]
      (send [:tell dst (decorate t d)]))))

(defn fail [k t]
  (log 'fail k t))

;;XXX use me, add line/col wherever begin/env/idx occur.
(defn advance [pos c]
  (update (if (= c \newline)
            (-> c (update :line inc) (assoc :col 1))
            (-> c (update :col inc)))
          :idx inc))

(defmethod init java.lang.Character [[i c :as k]]
  (let [x (when (< i (count input))
            (nth input i))
        t {::a/begin {:idx i}
           ::a/end {:idx (inc i)}
           ::a/value x}
        f (if (= x c) pass fail)]
    (f k t)))

(defn pass-empty [[i & _ :as k]]
  (log 'pass-empty k)
  (pass k {::a/begin {:idx i}
           ::a/end {:idx i}
           ::a/value []}))

(defmethod init 'ambiparse/cat [[i [_ & pats] :as k]]
  (if (seq pats)
    (let [cont (add-node i [:seq pats k])]
      (add-edge i (first pats) cont :single))
    (pass-empty k)))

(defmethod tell :root [k t]
  (log 'parsed! t)
  (when (= (-> t ::a/end :idx) (count input))
    (pass k t)))

(defmethod tell 'ambiparse/cat [k t]
  (pass k t))

(defmethod init :seq [k]
  nil)

(defmethod tell :seq [[i [_ pats dst]] t]
  (if (next pats)
    (let [e (-> t ::a/end :idx)]
      (add-edge e (second pats)
                [e [:seq (next pats) dst]]
                [:prefix t]))
    (send [:tell dst t])))

(defmethod init 'ambiparse/alt [[i [_ & pats] :as k]]
  (doseq [pat pats]
    (add-edge i pat k :identity)))

(defmethod tell 'ambiparse/alt [k t]
  (pass k t))

(defmethod init 'ambiparse/* [[i [_ pat] :as k]]
  (let [cont (add-node i [:rep pat k])]
    (add-edge i pat cont :single))
  (pass-empty k))

(defmethod init 'ambiparse/+ [[i [_ pat] :as k]]
  (let [cont (add-node i [:rep pat k])]
    (add-edge i pat cont :single)))

(defmethod tell 'ambiparse/* [k t]
  (pass k t))

(defmethod tell 'ambiparse/+ [k t]
  (pass k t))

(defmethod init :rep [k]
  nil)

(defmethod tell :rep [[i [_ pat dst]] t]
  (let [e (-> t ::a/end :idx)]
    (add-edge e pat
              [e [:rep pat dst]]
              [:prefix t]))
  (send [:tell dst t]))

(defmethod init 'ambiparse/-rule [[i [_ pat f] :as k]]
  (add-edge i pat k :identity))

(defmethod tell 'ambiparse/-rule [[i [_ pat f] :as k] t]
  (pass k (f t)))

(defmethod init 'ambiparse/label [[i [_ name pat] :as k]]
  (add-edge i pat k :identity))

(defmethod tell 'ambiparse/label [[i [_ name pat] :as k] t]
  (let [t* (select-keys t [::a/begin ::a/end ::a/value])] ; Strip labels.
    (pass k (assoc t* name (::a/value t)))))

(defmethod init clojure.lang.Var [[i pat :as k]]
  (add-edge i @pat k :identity))

(defmethod tell clojure.lang.Var [[i pat :as k] t]
  (pass k (assoc t ::a/var pat)))

(defn exec [[op & _ :as msg]]
  (log (list 'exec msg))
  (case op
    :init (let [[_ k] msg]
            (init k))
    :link (let [[_ src dst d] msg]
            (doseq [t (get-in graph (conj src :parses))]
              (send [:tell dst (decorate t d)])))
    :tell (let [[_ k t] msg]
            (tell k t))
    ))

(defn pump []
  (log 'pump)
  (let [q queue]
    (set! queue [])
    (run! exec q)))

(def root [0 :root])

(defn parses [pat s]
  (binding [input s
            graph {}
            queue []
            fuel fuel]
    (add-edge 0 pat root :identity)
    (while (seq queue)
      (when (zero? (update! fuel dec))
        (throw (Exception. "out of fuel!")))
      (pump))
    (log 'final-state= (state))
    (->> (get-in graph [0 :root :parses])
         (map ::a/value))))

(defn parse [pat s]
  (let [ps (distinct (parses pat s))]
    (cond
      (next ps) (throw (ex-info "Ambiguous parse:" {:parses (take 2 ps)}))
      (seq ps) (first ps)
      :else (throw (ex-info "Parse failed" {:error "TODO"}))))) ;XXX
