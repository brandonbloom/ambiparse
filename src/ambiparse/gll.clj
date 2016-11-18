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
(def ^:dynamic buffered)
(def ^:dynamic root)
(def ^{:dynamic true
       :doc "Vector mapping line minus one to index of previous newline."}
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
    (reduce-kv (fn [res n b]
                 (if (<= i b)
                   (reduced res)
                   {:idx i :line (inc n) :col (- i b -1)}))
               {:idx i :line 1 :col (inc i)}
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
   (if-let [ex (get-in graph (conj k :exception))]
     {::a/exception ex ::a/pos (pos-at i)}
     (cond
       (generated k) nil
       (< (count input) i) {::a/pos (pos-at i)
                            ::a/message "Unexpected end of input"}
       :else (-failure k)))))

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
        (send [:pass dst (decorate t d)])))
    k))

(defn pass [k t] ;XXX rename to "generate?" or "emit?"
  (when-not (get-in graph (conj k :generated t))
    (update! graph update-in (conj k :generated) conjs t)
    (doseq [[dst ds] (get-in graph (conj k :edges))
            d ds]
      (send [:pass dst (decorate t d)]))))

(defn input-at [i]
  (if (< i (count input))
    (nth input i)
    ::a/eof))


;;; Root.

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


;;; Terminals.

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
      {::a/pos (pos-at i)
       ::a/expected c
       ::a/actual x})))

(defn empty-at [i]
  (let [p (pos-at i)]
    {::a/begin p
     ::a/end p
     ::a/value []}))


;;; Concatenation.

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

(defn rightmost [kw xs]
  (when (seq xs)
    (apply max-key #(-> % kw :idx) xs)))

(defn rightmost-received [k]
  (rightmost ::a/end (received k)))

(defmethod -failure 'ambiparse/cat [[i [_ & pats] :as k]]
  (if-let [t (rightmost-received k)]
    (if-let [cont (::a/continue t)]
      (failure [(-> t ::a/end :idx) (first cont)]))
    (when-first [p pats]
      (failure [i p]))))


;;; Alternation.

(defmethod init 'ambiparse/alt [[i [_ & pats] :as k]]
  (doseq [pat pats]
    (add-edge i pat k nil)))

(defmethod passed 'ambiparse/alt [k t]
  (pass k t))

(defmethod -failure 'ambiparse/alt [[i [_ & pats]]]
  (let [errs (->> pats (map #(failure [i %])) (remove nil?))
        pos (->> errs (rightmost ::a/pos) ::a/pos)
        errs (filter #(= (::a/pos %) pos) errs)]
    (cond
      (next errs) {::a/alts (set errs)}
      (seq errs) (first errs))))


;;; Repetition.

(defn do-rep [[_ [_ pat] :as k] t]
  (pass k t)
  (let [i (-> t ::a/end :idx)]
    (add-edge i pat k {:prefix t})))

(defmethod init 'ambiparse/* [[i _ :as k]]
  (do-rep k (empty-at i)))

(defmethod init 'ambiparse/+ [[i [_ pat] :as k]]
  (add-edge i pat k {:prefix (empty-at i)}))

(defmethod passed 'ambiparse/* [k t]
  (do-rep k t))

(defmethod passed 'ambiparse/+ [k t]
  (do-rep k t))

(defmethod -failure 'ambiparse/* [k]
  ;;XXX If at end of string, get failure from rightmost+1 occurence of pattern.
  nil)

(defmethod -failure 'ambiparse/+ [[i [_ pat]]]
  (failure [i pat]))


;;; Optional.

(defmethod init 'ambiparse/? [[i [_ pat] :as k]]
  (let [t (empty-at i)]
    (pass k t)
    (add-edge i pat k {:prefix t})))

(defmethod passed 'ambiparse/? [k t]
  (pass k t))

(defmethod -failure 'ambiparse/? [k]
  ;;XXX If at end of string, get failure from pattern.
  nil)



;;; Transformation.

(defmethod init 'ambiparse/-rule [[i [_ pat f] :as k]]
  (add-edge i pat k nil))

(defmethod passed 'ambiparse/-rule [[i [_ pat f] :as k] t]
  (pass k (f t)))

(defmethod -failure 'ambiparse/-rule [[i [_ pat] :as k]]
  (failure [i pat]))


;;; Labeling.

(defmethod init 'ambiparse/label [[i [_ name pat] :as k]]
  (add-edge i pat k nil))

(defmethod passed 'ambiparse/label [[i [_ name pat] :as k] t]
  (let [t* (select-keys t [::a/begin ::a/end ::a/value])] ; Strip labels.
    (pass k (assoc t* name (::a/value t)))))

(defmethod -failure 'ambiparse/label [[i [_ _ pat]]]
  (failure [i pat]))


;;; Indirection.

(defmethod init clojure.lang.Var [[i pat :as k]]
  (add-edge i @pat k nil))

(defmethod passed clojure.lang.Var [[i pat :as k] t]
  (pass k (assoc t ::a/var pat)))

(defmethod -failure clojure.lang.Var [[i pat]]
  (some-> (failure [i @pat])
          (assoc ::a/var pat)))


;;; Precedence.

(defmethod init 'ambiparse/prefer [[i [_ cmp pat] :as k]]
  (add-edge i pat k nil))

(defmethod passed 'ambiparse/prefer [[i [_ cmp pat] :as k] t]
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

(defmethod -failure 'ambiparse/prefer [[i [_ _ pat]]]
  (failure [i pat]))


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
        (pass k t)))))

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
            buffered #{}
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
    (if (seq ps)
      [(first ps) (when (next ps)
                    {::a/message "Ambiguous" ::a/parses (take 2 ps)})]
      [nil (failure root)])))

(defn parse! []
  (let [[p err] (parse)]
    (when err
      (throw (ex-info "Parse failed" err)))
    p))

(comment

  (binding [breaks [0 3 7]]
    (doseq [i (range 9)]
      (prn i (pos-at i))))

)
