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

;;TODO: Combine in to single state record? Benchmark.

;;; Essential state.
(def ^:dynamic input)
(def ^:dynamic graph)
(def ^:dynamic queue)
(def ^:dynamic root)
(def ^{:dynamic true
       :doc "Vector mapping row index minus one to index of previous newline."}
  breaks)

;;; Debug state.
(def trace false)
(def ^:dynamic fuel
  "Steps to perform before giving up. Set to 0 to disable."
  200)

(defmacro log [& xs]
  (require 'fipp.edn)
  (when trace
    `(fipp.edn/pprint (list ~@xs) {:width 200})))

(defn state []
  {:input input
   :graph graph
   :queue queue
   :fuel fuel})

(defn pos-at [i]
  (if breaks
    ;;TODO: Binary search?
    (reduce-kv (fn [res r b]
                 (if (<= i b)
                   (reduced res)
                   {:idx i :row (inc r) :col (- i b -1)}))
               {:idx i :row 1 :col (inc i)}
               breaks)
    {:idx i}))

(defn classify [x]
  (cond
    (sequential? x) (first x)
    (keyword? x) x
    :else (class x)))

(defn dispatch [[i x]]
  (classify x))

;; Fully re-create multimethods for dev sanity.
(doseq [sym '[init passed -failure]]
  (ns-unmap *ns* sym))

(defmulti init
  "Called when a parse node for a given key is first created."
  dispatch)

(defmulti passed
  "Tells k about a successful sub-parse."
  (fn [k t] (dispatch k)))

(defmulti -failure
  "Asks k for a failure."
  dispatch)

(defn generated [k]
  (get-in graph (conj k :generated)))

(defn failure
  ([] (-failure root))
  ([[i & _ :as k]]
   (cond
     (generated k) nil
     (< (count input) i) {::pos (pos-at i)
                          ::message "Unexpected end of input"}
     :else (-failure k))))

(defn received [k]
  (get-in graph (conj k :received)))

(def conjs (fnil conj #{}))

(defn send [msg]
  ;(log 'send msg)
  (update! queue conj msg)
  nil)

(defn add-node [i pat]
  (let [k [i pat]]
    (when-not (get-in graph k)
      (update! graph assoc-in k {})
      (send [:init k]))
    k))

(defn decorate
  "Applies a transformation to trees flowing along an edge."
  [t {:as d :keys [prefix continue]}]
  (if d
    (merge prefix t
           {::a/begin (::a/begin prefix)
            ::a/end (::a/end t)
            ::a/value (conj (::a/value prefix) (::a/value t))}
           (when continue
             {::a/continue continue}))
    t))

(defn add-edge [i pat dst d]
  (let [k (add-node i pat)]
    (when-not (get-in graph (conj k :edges dst d))
      (update! graph update-in (conj k :edges dst) conjs d)
      ;; Replay previously generated parses.
      (doseq [t (get-in graph (conj k :generated))]
        (send [:pass dst (decorate t d)])
        (update! graph update-in (conj k :received) conjs t)))
    k))

(defn pass [k t] ;XXX rename to "generate?" or "emit?"
  (when-not (get-in graph (conj k :generated t))
    (update! graph update-in (conj k :generated) conjs t)
    (doseq [[dst ds] (get-in graph (conj k :edges))
            d ds]
      (send [:pass dst (decorate t d)]))))

(defmethod init :root [[i [_ pat] :as k]]
  (add-edge i pat k nil))

(defmethod passed :root [k t]
  ;;TODO: Move this filter to the sender to minimize message traffic.
  ;; Can be done recursively, so any node that is in some sort of
  ;; "tail position" can drop interior parse messages. h/t Mark Engelberg.
  ;; This will also eliminate the explicit root node completely.
  (log 'parsed? t)
  (when (= (-> t ::a/end :idx) (count input))
    (log 'parsed! t)
    (pass k t)))

(defmethod -failure :root [[i [_ pat]]]
  (failure [i pat]))

;;XXX use me, add line/col wherever begin/env/idx occur.
;;^^^ Idea: Vector of pairs [idx row] for fast row lookup + col calculation.
(defn advance [pos c]
  (update (if (= c \newline)
            (-> c (update :line inc) (assoc :col 1))
            (-> c (update :col inc)))
          :idx inc))

(defn input-at [i]
  (when (< i (count input))
    (nth input i)))

(defmethod init java.lang.Character [[i c :as k]]
  (let [x (input-at i)
        _ (when (and (= x \newline)
                     breaks
                     (< (peek breaks) i))
            (update! breaks conj i))
        t {::a/begin (pos-at i)
           ::a/end (pos-at (inc i))
           ::a/value x}]
    (when (= x c)
      (pass k t))))

(defmethod -failure java.lang.Character [[i c]]
  (let [x (input-at i)]
    (when (not= x c)
      {::pos (pos-at i)
       ::expected c
       ::actual x})))

(defn empty-at [i]
  (let [p (pos-at i)]
    {::a/begin p
     ::a/end p
     ::a/value []}))

(defn do-cat [t k pats]
  (if-let [[p & ps] pats]
    (let [i (-> t ::a/end :idx)
          d {:prefix t :continue ps}]
      (add-edge i p k d))
    (pass k t)))

(defmethod init 'ambiparse/cat [[i [_ & pats] :as k]]
  (do-cat (empty-at i) k pats))

(defmethod passed 'ambiparse/cat [k t]
  (do-cat (dissoc t ::a/continue) k (::a/continue t)))

(defmethod -failure 'ambiparse/cat [[i [_ & pats] :as k]]
  (if-let [t (when-let [rs (seq (received k))]
               (apply max-key #(-> % ::a/end :idx) rs))]
    (if-let [cont (::a/continue t)]
      (failure [(-> t ::a/end :idx) (first cont)]))
    (when-first [p pats]
      (failure [i p]))))

(defmethod init 'ambiparse/alt [[i [_ & pats] :as k]]
  (doseq [pat pats]
    (add-edge i pat k nil)))

(defmethod passed 'ambiparse/alt [k t]
  (pass k t))

(defmethod -failure 'ambiparse/alt [k]
  :todo-alt-failure) ;XXX

(defn do-rep [[_ [_ pat] :as k] t]
  (pass k t)
  (let [i (-> t ::a/end :idx)]
    (add-edge i pat k {:prefix t})))

(defmethod init 'ambiparse/* [[i _ :as k]]
  (do-rep k (empty-at i)))

(defmethod init 'ambiparse/+ [[i [_ pat] :as k]]
  (add-edge i pat k {:prefix (empty-at i)}))

(defmethod -failure 'ambiparse/+ [k]
  :todo-plus-failure) ;XXX

(defmethod passed 'ambiparse/* [k t]
  (do-rep k t))

(defmethod passed 'ambiparse/+ [k t]
  (do-rep k t))

(defmethod init 'ambiparse/-rule [[i [_ pat f] :as k]]
  (add-edge i pat k nil))

(defmethod passed 'ambiparse/-rule [[i [_ pat f] :as k] t]
  (pass k (f t)))

(defmethod init 'ambiparse/label [[i [_ name pat] :as k]]
  (add-edge i pat k nil))

(defmethod passed 'ambiparse/label [[i [_ name pat] :as k] t]
  (let [t* (select-keys t [::a/begin ::a/end ::a/value])] ; Strip labels.
    (pass k (assoc t* name (::a/value t)))))

(defmethod init clojure.lang.Var [[i pat :as k]]
  (add-edge i @pat k nil))

(defmethod passed clojure.lang.Var [[i pat :as k] t]
  (pass k (assoc t ::a/var pat)))

(defn exec [[op & _ :as msg]]
  (log 'exec msg)
  (case op
    :init (let [[_ k] msg]
            (init k))
    :pass (let [[_ k t] msg]
            (when-not (get-in graph (conj k :received t))
              (passed k t)))
    ))

(defn pump []
  (log 'pump)
  (let [q queue]
    (set! queue [])
    (doseq [msg q]
      (when (zero? (update! fuel dec))
        (throw (Exception. "out of fuel!")))
      (exec msg))))

(defn run []
  (apply add-node root)
  (try
    (while (seq queue)
      (pump))
    (finally
      (log 'final-state= (state))
      ;(ambiparse.viz/show! graph)
      )))

(defn with-run-fn [pat s f]
  (binding [input s
            root [0 [:root pat]]
            graph []
            queue []
            breaks (when (string? s) [])
            fuel fuel]
    (run)
    (f)))

(defmacro with-run [pat s & body]
  `(with-run-fn ~pat ~s (fn [] ~@body)))

(defn parses
  ([] (parses root))
  ([k] (->> k generated (map ::a/value))))

(defn parse []
  (let [ps (distinct (parses))]
    (cond
      (next ps) (throw (ex-info "Ambiguous parse:" {::a/parses (take 2 ps)}))
      (seq ps) (first ps)
      :else (throw (ex-info "Parse failed" (failure root))))))

(comment

  (binding [breaks [0 3 7]]
    (doseq [i (range 9)]
      (prn i (pos-at i))))

)
