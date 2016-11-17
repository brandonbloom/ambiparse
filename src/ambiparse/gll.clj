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

;;TODO: Combine in to single state record.

;;; Essential state.
(def ^:dynamic input)
(def ^:dynamic graph)
(def ^:dynamic queue)

;;; Debug state.
(def trace false)
(def ^:dynamic fuel
  "Steps to perform before giving up. Set to 0 to disable."
  0)

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

;; Fully re-create multimethods for dev sanity.
(doseq [sym '[init decorate passed failed]]
  (ns-unmap *ns* sym))

(defmulti init
  "Called when a parse node for a given key is first created."
  dispatch)

(defmulti decorate
  "Applies a transformation to trees flowing along an edge."
  (fn [t by] (classify by)))

(defmulti passed
  "Tells k about a successful sub-parse."
  (fn [k t] (dispatch k)))

(defmulti failure
  "Asks k for a failure."
  (fn [k err] (dispatch k)))

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

;;TODO: Is this the only decorator needed?
(defmethod decorate :prefix [x [_ xs]]
  (merge xs x {::a/begin (::a/begin xs)
               ::a/end (::a/end x)
               ::a/value (conj (::a/value xs) (::a/value x))}))

(defn pass [k t]
  (when-not (get-in graph (conj k :parses t))
    (update! graph update-in (conj k :parses) conjs t)
    (doseq [[dst ds] (get-in graph (conj k :edges))
            d ds]
      (send [:pass dst (decorate t d)]))))

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
           ::a/value x}]
    (when (= x c)
      (pass k t))))

(defn empty-at [i]
  {::a/begin {:idx i}
   ::a/end {:idx i}
   ::a/value []})

(defn pass-empty [[i & _ :as k]]
  (pass k (empty-at i)))

(defmethod init 'ambiparse/cat [[i [_ & pats] :as k]]
  (if (seq pats)
    (let [cont (add-node i [:seq pats k])]
      (add-edge i (first pats) cont [:prefix (empty-at i)]))
    (pass-empty k)))

(defmethod passed :root [k t]
  (log 'parsed! t)
  ;;TODO: Move this filter to the sender to minimize message traffic.
  ;; Can be done recursively, so any node that is in some sort of
  ;; "tail position" can drop interior parse messages. h/t Mark Engelberg.
  (when (= (-> t ::a/end :idx) (count input))
    (pass k t)))

(defmethod passed 'ambiparse/cat [k t]
  (pass k t))

(defmethod init :seq [k]
  nil)

(defmethod passed :seq [[i [_ pats dst]] t]
  (if (next pats)
    (let [e (-> t ::a/end :idx)]
      (add-edge e (second pats)
                [e [:seq (next pats) dst]]
                [:prefix t]))
    (send [:pass dst t])))

(defmethod init 'ambiparse/alt [[i [_ & pats] :as k]]
  (doseq [pat pats]
    (add-edge i pat k :identity)))

(defmethod passed 'ambiparse/alt [k t]
  (pass k t))

(defmethod init 'ambiparse/* [[i [_ pat] :as k]]
  (let [cont (add-node i [:rep pat k])]
    (add-edge i pat cont [:prefix (empty-at i)]))
  (pass-empty k))

(defmethod init 'ambiparse/+ [[i [_ pat] :as k]]
  (let [cont (add-node i [:rep pat k])]
    (add-edge i pat cont [:prefix (empty-at i)])))

(defmethod passed 'ambiparse/* [k t]
  (pass k t))

(defmethod passed 'ambiparse/+ [k t]
  (pass k t))

(defmethod init :rep [k]
  nil)

(defmethod passed :rep [[i [_ pat dst]] t]
  (let [e (-> t ::a/end :idx)]
    (add-edge e pat
              [e [:rep pat dst]]
              [:prefix t]))
  (send [:pass dst t]))

(defmethod init 'ambiparse/-rule [[i [_ pat f] :as k]]
  (add-edge i pat k :identity))

(defmethod passed 'ambiparse/-rule [[i [_ pat f] :as k] t]
  (pass k (f t)))

(defmethod init 'ambiparse/label [[i [_ name pat] :as k]]
  (add-edge i pat k :identity))

(defmethod passed 'ambiparse/label [[i [_ name pat] :as k] t]
  (let [t* (select-keys t [::a/begin ::a/end ::a/value])] ; Strip labels.
    (pass k (assoc t* name (::a/value t)))))

(defmethod init clojure.lang.Var [[i pat :as k]]
  (add-edge i @pat k :identity))

(defmethod passed clojure.lang.Var [[i pat :as k] t]
  (pass k (assoc t ::a/var pat)))

(defn exec [[op & _ :as msg]]
  (log (list 'exec msg))
  (case op
    :init (let [[_ k] msg]
            (init k))
    :link (let [[_ src dst d] msg]
            (doseq [t (get-in graph (conj src :parses))]
              (send [:pass dst (decorate t d)])))
    :pass (let [[_ k t] msg]
            (passed k t))
    ))

(defn pump []
  (log 'pump)
  (let [q queue]
    (set! queue [])
    (run! exec q)))

(def root [0 :root])

(defn run [pat]
  (add-edge 0 pat root :identity)
  (while (seq queue)
    (when (zero? (update! fuel dec))
      (throw (Exception. "out of fuel!")))
    (pump))
  (log 'final-state= (state)))

(defn with-run-fn [pat s f]
  (binding [input s
            graph []
            queue []
            fuel fuel]
    (run pat)
    (f)))

(defmacro with-run [pat s & body]
  `(with-run-fn ~pat ~s (fn [] ~@body)))

(defn successes [k]
  (get-in graph (conj k :parses)))

(defn parses []
  (->> root successes (map ::a/value)))

(defn parse []
  (let [ps (distinct (parses))]
    (cond
      (next ps) (throw (ex-info "Ambiguous parse:" {::a/parses (take 2 ps)}))
      (seq ps) (first ps)
      :else (throw (ex-info "Parse failed" (failure root))))))
