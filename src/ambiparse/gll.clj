(ns ambiparse.gll
  (:refer-clojure :exclude [send])
  (:require [clojure.spec :as s]
            [ambiparse.util :refer :all]))

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
(def ^:dynamic buffered)
(def ^:dynamic root)
(def ^{:dynamic true
       :doc "Vector mapping line minus one to index of previous newline."}
  breaks)
(def ^{:dynamic true
       :doc "Furthest index into the input examined."}
  traveled)

;;; Debug state.
(def trace false)
(def ^:dynamic fuel
  "Steps to perform before giving up. Set to 0 to disable."
  0)

(defmacro log [& xs]
  (require 'fipp.edn)
  (when trace
    `(fipp.edn/pprint (list ~@xs) {:width 200})))

(defn state []
  {:input input
   :graph graph
   :root root
   :queue queue
   :fuel fuel})

(defn pos-at [i]
  (if breaks
    ;;TODO: Binary search?
    (reduce-kv (fn [res n b]
                 (if (<= i b)
                   (reduced res)
                   {:idx i :line (inc n) :col (- i b -1)}))
               {:idx i :line 1 :col (inc i)}
               breaks)
    {:idx i}))

(defn classify [pat]
  (if (sequential? pat)
    (first pat)
    (class pat)))

(defn dispatch [[i pat tail?]]
  (classify pat))

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

(def ^:dynamic inside)

(defn failure
  ([]
   (binding [inside #{}]
     (failure root)))
  ([[i _ _ :as k]]
   (when-not (inside k)
     (binding [inside (conj inside k)]
       (if-let [ex (get-in graph (conj k :exception))]
         {::a/exception ex ::a/pos (pos-at i)}
         (-failure k))))))

(defn received [k]
  (get-in graph (conj k :received)))

(def conjs (fnil conj #{}))

(defn send [msg]
  ;(log 'send msg)
  (update! queue conj msg)
  nil)

(defn add-node [i pat tail?]
  (let [k [i pat tail?]]
    (when-not (get-in graph k)
      (update! graph assoc-in k {:tail? tail?})
      (send [:init k]))
    k))

(defn decorate
  "Applies a transformation to trees flowing along an edge."
  [pat t {:as d :keys [prefix continue]}]
  (if d
    (merge prefix t
           {::a/begin (::a/begin prefix)
            ::a/end (::a/end t)
            ::a/children (conj (::a/children prefix) t)
            ::a/value (conj (::a/value prefix) (::a/value t))
            ::a/elements (conj (::a/elements prefix) t)}
           (when continue
             {::a/continue continue}))
    t))

(defn add-edge [i pat tail? dst d]
  (let [k (add-node i pat tail?)]
    (when-not (get-in graph (conj k :edges dst d))
      (update! graph update-in (conj k :edges dst) conjs d)
      ;; Replay previously generated parses.
      (doseq [t (get-in graph (conj k :generated))]
        (send [:pass dst (decorate pat t d)])))
    k))

(defn pass [[_ pat tail? :as k] t] ;XXX rename to "generate?" or "emit?"
  (when (or (not tail?)
            (= (-> t ::a/end :idx) (count input)))
    (let [t (assoc t ::a/pattern pat)]
      (when-not (get-in graph (conj k :generated t))
        (update! graph update-in (conj k :generated) conjs t)
        (doseq [[dst ds] (get-in graph (conj k :edges))
                d ds]
          (send [:pass dst (decorate pat t d)]))))))

(defn pass-child [k t]
  (pass k (assoc t ::a/children [t])))

(defn maybe-break [i c]
  (assert (<= i (inc traveled)))
  (update! traveled max traveled i)
  (when (and (= c \newline)
             breaks
             (< (peek breaks) i))
    (update! breaks conj i)))

(defn input-at [i]
  (if (< i (count input))
    (let [c (nth input i)]
      (maybe-break i c)
      c)
    ::a/eof))

(defn empty-at [i]
  (let [p (pos-at i)]
    {::a/begin p
     ::a/end p
     ::a/children []
     ::a/elements []
     ::a/value []}))

(defn item-at [i]
  )


;;; Terminals.

(defn lit-init [i c k]
  (let [x (input-at i)]
    (when (= x c)
      (pass k {::a/begin (pos-at i)
               ::a/end (pos-at (inc i))
               ::a/value x}))))

(defn lit-failure [i c]
  (let [x (input-at i)]
    (when (not= x c)
      {::a/pos (pos-at i)
       ::a/expected c
       ::a/actual x})))

(defmethod init 'ambiparse/lit [[i [_ c] _ :as k]]
  (lit-init i c k))

(defmethod -failure 'ambiparse/lit [[i [_ c] _]]
  (lit-failure i c))

(defmethod init java.lang.Character [[i c _ :as k]]
  (lit-init i c k))

(defmethod -failure java.lang.Character [[i c _]]
  (lit-failure i c))

(defmethod init java.lang.String [[i s _ :as k]]
  (loop [n 0]
    (if (< n (count s))
      (when (= (input-at (+ i n)) (nth s n))
        (recur (inc n)))
      (pass k {::a/begin (pos-at i)
               ::a/end (pos-at (+ i n))
               ::a/value s}))))

(defmethod -failure java.lang.String [[i s tail?]]
  (loop [n 0]
    (if (and (< n (count s)) (= (input-at (+ i n)) (nth s n)))
      (recur (inc n))
      (let [actual (subs input i (min (count input) (+ i (count s))))]
        {::a/pos (pos-at (+ i n))
         ::a/expected s
         ::a/actual actual}))))

(defmethod init 'ambiparse/-pred [[i [_ _ f] _ :as k]]
  (let [x (input-at i)]
    (when (f x)
      (pass k {::a/begin (pos-at i)
               ::a/end (pos-at (inc i))
               ::a/value x}))))

(defmethod -failure 'ambiparse/-pred [[i [_ expr f] _ :as k]]
  (let [x (input-at i)]
    (when-not (f x)
      {::a/message "Predicate failed"
       ::a/pos (pos-at i)
       ::a/predicate f
       ::a/expression expr
       ::a/actual x})))


;;; Concatenation.

(defn do-cat [t [_ _ tail? :as k] pats]
  (if-let [[p & ps] pats]
    (let [i (-> t ::a/end :idx)
          d {:prefix t :continue ps}
          tl? (and (empty? ps) tail?)]
      (add-edge i p tl? k d))
    (pass k (assoc t ::a/structure (second k)))))

(defmethod init 'ambiparse/cat [[i [_ & pats] tail? :as k]]
  (do-cat (empty-at i) k pats))

(defmethod passed 'ambiparse/cat [k t]
  (do-cat (dissoc t ::a/continue) k (::a/continue t)))

(defn rightmost [kw xs]
  (when (seq xs)
    (apply max-key #(-> % kw :idx) xs)))

(defn rightmost-received [k]
  (rightmost ::a/end (received k)))

(defmethod -failure 'ambiparse/cat [[i [_ & pats] tail? :as k]]
  (if-let [t (rightmost-received k)]
    (if-let [cont (::a/continue t)]
      (failure [(-> t ::a/end :idx) (first cont)]))
    (when-first [p pats]
      (failure [i p tail?]))))


;;; Alternation.

(defmethod init 'ambiparse/alt [[i [_ & pats] tail? :as k]]
  (doseq [pat pats]
    (add-edge i pat tail? k nil)))

(defmethod passed 'ambiparse/alt [k t]
  (pass-child k t))

(defmethod -failure 'ambiparse/alt [[i [_ & pats] tail?]]
  (let [errs (->> pats (map #(failure [i % tail?])) (remove nil?))
        pos (->> errs (rightmost ::a/pos) ::a/pos)
        errs (filter #(= (::a/pos %) pos) errs)]
    (cond
      (next errs) {::a/pos (pos-at i) ::a/alts (set errs)}
      (seq errs) (first errs))))


;;; Repetition.

(defn do-rep [[_ [_ pat] tail? :as k] t]
  (pass k t)
  (let [i (-> t ::a/end :idx)]
    (add-edge i pat false k {:prefix t})))

(defmethod init 'ambiparse/* [[i _ tail? :as k]]
  (do-rep k (empty-at i)))

(defmethod init 'ambiparse/+ [[i [_ pat] tail? :as k]]
  (add-edge i pat false k {:prefix (empty-at i)}))

(defmethod passed 'ambiparse/* [k t]
  (do-rep k t))

(defmethod passed 'ambiparse/+ [k t]
  (do-rep k t))

(defn rep-failure [[i [_ pat] tail? :as k]]
  (when tail?
    (let [e (or (-> (rightmost-received k) ::a/end :idx) i)]
      (failure [e pat false]))))

(defmethod -failure 'ambiparse/* [k]
  (rep-failure k))

(defmethod -failure 'ambiparse/+ [[i [_ pat] tail? :as k]]
  (or (failure [i pat false])
      (rep-failure k)))


;;; Optional.

(defmethod init 'ambiparse/? [[i [_ pat] tail? :as k]]
  (let [t (empty-at i)]
    (pass k t)
    (add-edge i pat tail? k {:prefix t})))

(defmethod passed 'ambiparse/? [k t]
  (pass k t))

(defmethod -failure 'ambiparse/? [k]
  ;;XXX If at end of string, get failure from pattern.
  nil)


;;; Transformation.

(defmethod init 'ambiparse/-rule [[i [_ pat _ f] tail? :as k]]
  (add-edge i pat tail? k nil))

(defmethod passed 'ambiparse/-rule [[i [_ pat _ f] tail? :as k] t]
  (pass-child k (f t)))

(defmethod -failure 'ambiparse/-rule [[i [_ pat expr _] tail? :as k]]
  ;;XXX Use expr if the rule failed.
  (failure [i pat]))


;;; Labeling.

(defmethod init 'ambiparse/label [[i [_ name pat] tail? :as k]]
  (add-edge i pat tail? k nil))

(defn strip-labels [t]
  (->> t (filter (fn [[k v]] (= (namespace k) "ambiparse"))) (into {})))

(defmethod passed 'ambiparse/label [[i [_ name pat] tail? :as k] t]
  (pass-child k (assoc (strip-labels t) name (::a/value t))))

(defmethod -failure 'ambiparse/label [[i [_ _ pat]]]
  (failure [i pat]))


;;; Indirection.

(defmethod init clojure.lang.Var [[i pat tail? :as k]]
  (add-edge i @pat tail? k nil))

(defmethod passed clojure.lang.Var [[i pat tail? :as k] t]
  (pass-child k (assoc t ::a/var pat)))

(defmethod -failure clojure.lang.Var [[i pat]]
  (some-> (failure [i @pat])
          (assoc ::a/var pat)))


;;; Precedence.

(defmethod init 'ambiparse/-prefer [[i [_ _ pat cmp] tail? :as k]]
  (add-edge i pat tail? k nil))

(defmethod passed 'ambiparse/-prefer [[i [_ _ pat cmp] tail? :as k] t]
  ;;XXX set exception if cmp fails.
  (let [buffer (get-in graph (conj k :buffer))
        buffer* (cond
                  ;; First parse.
                  (empty? buffer) #{t}
                  ;; Strictly preferrable.
                  (every? #(neg? (cmp % t)) buffer) #{t}
                  ;; Not strictly less preferrable.
                  ;;TODO: Compare in one pass over buffer.
                  (some #(zero? (cmp % t)) buffer) (conjs buffer t))]
    (when buffer*
      (update! graph assoc-in (conj k :buffer) buffer*)
      (update! buffered conj k))))

(defmethod -failure 'ambiparse/-prefer [[i [_ _ pat]]]
  (failure [i pat]))


;;; Filtering.
;;TODO: remove seems more common - better primative?

(defmethod init 'ambiparse/-filter [[i [_ _ pat _] tail? :as k]]
  (add-edge i pat tail? k nil))

(defmethod passed 'ambiparse/-filter [[i [_ _ pat f] tail? :as k] t]
  (when (f t)
    (pass-child k t)))

(defmethod -failure 'ambiparse/-filter [[i [_ expr pat f] tail? :as k]]
  (if-let [rs (received k)]
    {::a/message "Filter predicate failed"
     ::a/predicate f
     ::a/expression expr
     ::a/pos (pos-at i)
     ::a/candidates rs}
    (failure [i pat])))


;;; Execution.

(defn exec [[op k & args :as msg]]
  (log 'exec msg)
  (try
    (case op
      :init (init k)
      :pass (let [[t] args]
              (when-not (get-in graph (conj k :received t))
                (passed k t)
                (update! graph update-in (conj k :received) conjs t))))
    (catch Exception ex
      (log 'catch-at k ex)
      (update! graph update-in (conj k :exception) #(or % ex)))))

(defn pump []
  (log 'pump)
  ;; Execute queued messages.
  (let [q queue]
    (set! queue [])
    (doseq [msg q]
      (when (zero? (update! fuel dec))
        (throw (Exception. "out of fuel!")))
      (exec msg)))
  ;; When subgraphs quiesce, flush buffers.
  (when (empty? queue)
    (log 'quiescence)
    (when-let [q (seq buffered)]
      (set! buffered #{})
      (doseq [k q
              :when (not (get-in graph (conj k :exception)))
              t (get-in graph (conj k :buffer))]
        (pass-child k t)
        ;;XXX clear the buffer!
        ))))

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
            root [0 pat true]
            graph []
            queue []
            buffered #{}
            breaks (when (string? s) [])
            traveled 0
            fuel fuel]
    (run)
    (f)))

(defmacro with-run [pat s & body]
  `(with-run-fn ~pat ~s (fn [] ~@body)))

(defn trees []
  (generated root))

(defn parses []
  (map ::a/value (trees)))

(defn parse []
  (let [ps (distinct (parses))]
    (if (seq ps)
      [(first ps) (when (next ps)
                    {::a/message "Ambiguous" ::a/parses (take 2 ps)})]
      [nil (or (failure) (throw (Exception. "Unknown failure")))])))

(defn parse! []
  (let [[p err] (parse)]
    (when err
      (throw (ex-info "Parse failed" err)))
    p))
