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

(s/def ::pos (s/keys :req-un [::idx] :opt-un [::line ::col]))
(s/def ::line integer?)
(s/def ::col integer?)
(s/def ::idx integer?)

(s/def ::env (s/map-of var? (s/coll-of ::pat, :kind set?)))

(s/def ::pattern any?)

(s/def ::a/begin ::pos)
(s/def ::a/end ::pos)
(s/def ::a/children (s/coll-of ::tree :kind vector?))
(s/def ::a/structure ::pattern)
(s/def ::a/elements (s/coll-of ::tree :kind vector?))
(s/def ::a/env ::env)
(s/def ::a/continue (s/coll-of ::pattern))

(s/def ::tree
  (s/keys :req [::a/begin ::a/end ::a/value ::a/env]
          :opt [::a/children ::a/structure ::a/elements ::a/continue]))

;;XXX Make a record!
(s/def ::key (s/spec (s/cat :i integer?,
                            :pat ::pattern,
                            :tail? boolean?
                            :env ::env)))

(defn scan-breaks [i]
  (when (< traveled i)
    (doseq [n (range traveled (min (inc i) (dec (count input))))]
      (when (and (= (nth input i) \newline)
                 breaks
                 (< (peek breaks) n))
        (change! breaks conj n)))
    (set! traveled i)))

(defn input-at [i]
  (if (< i (count input))
    (do (scan-breaks i)
        (nth input i))
    ::a/eof))

(defn pos-at [i]
  (scan-breaks i)
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

(defn dispatch [[i pat tail? env]]
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

(s/fdef failure :args (s/alt :root (s/cat) :specific (s/cat :k ::key)))

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

(defn send [msg]
  ;(log 'send msg)
  (change! queue conj msg)
  nil)

(s/fdef add-node
  :args (s/cat :i integer?, :pat ::pattern, :tail? boolean?, :env ::env))

(defn head-fail [i pat]
  (if (char? pat)
    (not= (input-at i) pat)
    (when-let [f (-> pat meta ::a/head-fail)]
      (f (input-at i)))))

(defn add-node [i pat tail? env]
  (when-not (head-fail i pat)
    (let [k [i pat tail? env]]
      (when-not (get-in graph k)
        (change! graph assoc-in k {:tail? tail? :env env})
        (send [:init k]))
      k)))

(s/def ::prefix ::tree)
(s/def ::continue (s/nilable (s/coll-of ::pattern :kind seq?)))

(s/def ::decorator
  (s/keys :req-un [::prefix]
          :opt-un [::continue]))

(s/fdef decorate
  :args (s/cat :pat ::pattern, :t ::tree, ::d (s/nilable ::decorator))
  :ret ::tree)

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

(s/fdef add-edge
  :args (s/cat :i integer?
               :pat any?
               :tail? boolean?
               :env ::env
               :dst ::key
               :d (s/nilable ::decorator)))

(defn add-edge [i pat tail? env dst d]
  (when-let [k (add-node i pat tail? env)]
    (when-not (get-in graph (conj k :edges dst d))
      (change! graph update-in (conj k :edges dst) conjs d)
      ;; Replay previously generated parses.
      (doseq [t (get-in graph (conj k :generated))]
        (send [:pass dst (decorate pat t d)])))))

(s/fdef pass
  :args (s/cat :k ::key, :t ::tree))

(defn pass [[_ pat tail? env :as k] t] ;XXX rename to "generate?" or "emit?"
  (when (or (not tail?)
            (= (-> t ::a/end :idx) (count input)))
    (let [t (assoc t ::a/pattern pat)]
      (when-not (get-in graph (conj k :generated t))
        (change! graph update-in (conj k :generated) conjs t)
        (doseq [[dst ds] (get-in graph (conj k :edges))
                d ds]
          (send [:pass dst (decorate pat t d)]))))))

(defn pass-child [k t]
  (pass k (assoc t ::a/children [t])))

(defn empty-at [i env]
  (let [p (pos-at i)]
    {::a/begin p
     ::a/end p
     ::a/children []
     ::a/elements []
     ::a/value []
     ::a/env env}))

(defn report-ex [k ex]
  (log 'catch-at k ex)
  (change! graph update-in (conj k :exception) #(or % ex))
  nil)

(defmacro try-at [k & body]
  `(let [k# ~k]
     (try
       ~@body
       (catch ~'Exception ex#
         (report-ex k# ex#)))))

(defn report-ex [k ex]
  (log 'catch-at k ex)
  (change! graph update-in (conj k :exception) #(or % ex))
  nil)

(defmacro try-at [k & body]
  `(let [k# ~k]
     (try
       ~@body
       (catch ~'Exception ex#
         (report-ex k# ex#)))))


;;; Terminals.

(defn lit-init [i c [_ _ _ env :as k]]
  (let [x (input-at i)]
    (when (= x c)
      (pass k {::a/begin (pos-at i)
               ::a/end (pos-at (inc i))
               ::a/value x
               ::a/env env}))))

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

(defmethod init java.lang.String [[i s _ env :as k]]
  (loop [n 0]
    (if (< n (count s))
      (when (= (input-at (+ i n)) (nth s n))
        (recur (inc n)))
      (pass k {::a/begin (pos-at i)
               ::a/end (pos-at (+ i n))
               ::a/value s
               ::a/env env}))))

(defmethod -failure java.lang.String [[i s tail? env]]
  (loop [n 0]
    (if (and (< n (count s)) (= (input-at (+ i n)) (nth s n)))
      (recur (inc n))
      (let [actual (subs input i (min (count input) (+ i (count s))))]
        {::a/pos (pos-at (+ i n))
         ::a/expected s
         ::a/actual actual}))))

(defmethod init 'ambiparse/-pred [[i [_ _ f] _ env :as k]]
  (let [x (input-at i)]
    (when (try-at k (f x))
      (pass k {::a/begin (pos-at i)
               ::a/end (pos-at (inc i))
               ::a/value x
               ::a/env env}))))

(defmethod -failure 'ambiparse/-pred [[i [_ expr f] _ :as k]]
  (let [x (input-at i)]
    (when-not (f x)
      {::a/message "Predicate failed"
       ::a/pos (pos-at i)
       ::a/predicate f
       ::a/expression expr
       ::a/actual x})))


;;; Concatenation.

(defn do-cat [t [_ _ tail? _ :as k] pats]
  (if-let [[p & ps] pats]
    (let [i (-> t ::a/end :idx)
          env (::a/env t)
          d {:prefix t :continue ps}
          tl? (and (empty? ps) tail?)]
      (add-edge i p tl? env k d))
    (pass k (assoc t ::a/structure (second k)))))

(defmethod init 'ambiparse/cat [[i [_ & pats] tail? env :as k]]
  (do-cat (empty-at i env) k pats))

(defmethod passed 'ambiparse/cat [k t]
  (do-cat (dissoc t ::a/continue) k (::a/continue t)))

(defn rightmost [kw xs]
  ;XXX return _all_ rightmost, otherwise nested alts mask other alts.
  (when (seq xs)
    (apply max-key #(-> % kw :idx) xs)))

(defn rightmost-received [k]
  (rightmost ::a/end (received k)))

(defmethod -failure 'ambiparse/cat [[i [_ & pats] tail? env :as k]]
  (if-let [t (rightmost-received k)]
    (when-let [cont (::a/continue t)]
      (failure [(-> t ::a/end :idx) (first cont) tail? env]))
    (when-first [p pats]
      (failure [i p tail? env]))))


;;; Alternation.

(defmethod init 'ambiparse/alt [[i [_ & pats] tail? env :as k]]
  (doseq [pat pats]
    (add-edge i pat tail? env k nil)))

(defmethod passed 'ambiparse/alt [k t]
  (pass-child k t))

(defmethod -failure 'ambiparse/alt [[i [_ & pats] tail? env]]
  (let [errs (->> pats (map #(failure [i % tail? env])) (remove nil?))
        pos (->> errs (rightmost ::a/pos) ::a/pos)
        errs (filter #(= (::a/pos %) pos) errs)]
    (cond
      (next errs) {::a/pos (pos-at i) ::a/alts (set errs)}
      (seq errs) (first errs))))


;;; Repetition.

(defn do-rep [[_ [_ pat] tail? _ :as k] t]
  (pass k t)
  (let [i (-> t ::a/end :idx)
        env (::a/env t)]
    (add-edge i pat false env k {:prefix t})))

(defmethod init 'ambiparse/* [[i _ tail? env :as k]]
  (do-rep k (empty-at i env)))

(defmethod init 'ambiparse/+ [[i [_ pat] tail? env :as k]]
  (add-edge i pat false env k {:prefix (empty-at i env)}))

(defmethod passed 'ambiparse/* [k t]
  (do-rep k t))

(defmethod passed 'ambiparse/+ [k t]
  (do-rep k t))

(defn rep-failure [[i [_ pat] tail? env :as k]]
  (when tail?
    (let [e (or (-> (rightmost-received k) ::a/end :idx) i)]
      (failure [e pat false env]))))

(defmethod -failure 'ambiparse/* [k]
  (rep-failure k))

(defmethod -failure 'ambiparse/+ [[i [_ pat] tail? env :as k]]
  (or (failure [i pat false env])
      (rep-failure k)))


;;; Optional.

(defmethod init 'ambiparse/? [[i [_ pat] tail? env :as k]]
  (let [t (empty-at i env)]
    (pass k t)
    (add-edge i pat tail? env k {:prefix t})))

(defmethod passed 'ambiparse/? [k t]
  (pass k t))

(defmethod -failure 'ambiparse/? [[i [_ pat] tail? env]]
  (when tail?
    (failure [i pat tail? env])))


;;; Transformation.

(def ^:dynamic muts)

(defn modify [env]
  (reduce (fn [env [op v pat]]
            (case op
              :add (update env v conjs pat)
              :del (update env v disj pat)))
          env
          muts))

(defmethod init 'ambiparse/-rule [[i [_ pat _ f] tail? env :as k]]
  (add-edge i pat tail? env k nil))

(defmethod passed 'ambiparse/-rule [[i [_ pat _ f] tail? env :as k] t]
  (binding [muts []]
    (when-let [t* (try-at k (f t))]
      (let [t* (assoc t* ::a/env (modify env))]
        (pass-child k t*)))))

(defmethod -failure 'ambiparse/-rule [[i [_ pat expr _] tail? env :as k]]
  ;;XXX Use expr if the rule failed.
  (failure [i pat tail? env]))


;;; Labeling.

(defmethod init 'ambiparse/label [[i [_ name pat] tail? env :as k]]
  (add-edge i pat tail? env k nil))

(defn strip-labels [t]
  (->> t (filter (fn [[k v]] (= (namespace k) "ambiparse"))) (into {})))

(defmethod passed 'ambiparse/label [[i [_ name pat] tail? env :as k] t]
  (pass-child k (assoc (strip-labels t) name (::a/value t))))

(defmethod -failure 'ambiparse/label [[i [_ _ pat] tail? env]]
  (failure [i pat tail? env]))


;;; Indirection.

(defmethod init clojure.lang.Var [[i pat tail? env :as k]]
  (let [p @pat
        p (if (env pat) (list* `a/alt p (env pat)) p)]
    (add-edge i p tail? env k nil)))

(defmethod passed clojure.lang.Var [[i pat tail? env :as k] t]
  (pass-child k (assoc t ::a/var pat)))

(defmethod -failure clojure.lang.Var [[i pat tail? env]]
  (some-> (failure [i @pat tail? env])
          (assoc ::a/var pat)))


;;; Precedence.

(defmethod init 'ambiparse/-prefer [[i [_ _ pat cmp] tail? env :as k]]
  (add-edge i pat tail? env k nil))

(defmethod passed 'ambiparse/-prefer [[i [_ _ pat cmp] tail? env :as k] t]
  ;;XXX set exception if cmp fails.
  (let [buffer (get-in graph (conj k :buffer))
        buffer* (try-at k
                  (cond
                    ;; First parse.
                    (empty? buffer) #{t}
                    ;; Strictly preferrable.
                    (every? #(neg? (cmp % t)) buffer) #{t}
                    ;; Not strictly less preferrable.
                    ;;TODO: Compare in one pass over buffer.
                    (some #(zero? (cmp % t)) buffer) (conjs buffer t)))]
    (when buffer*
      (change! graph assoc-in (conj k :buffer) buffer*)
      (change! buffered conj k))))

(defmethod -failure 'ambiparse/-prefer [[i [_ _ pat] tail? env]]
  (failure [i pat tail? env]))


;;; Filtering.
;;TODO: remove seems more common - better primative?

(defmethod init 'ambiparse/-filter [[i [_ _ pat _] tail? env :as k]]
  (add-edge i pat tail? env k nil))

(defmethod passed 'ambiparse/-filter [[i [_ _ pat f] tail? env :as k] t]
  (when (try-at k (f t))
    (pass-child k t)))

(defmethod -failure 'ambiparse/-filter [[i [_ expr pat f] tail? env :as k]]
  (if-let [rs (received k)]
    {::a/message "Filter predicate failed"
     ::a/predicate f
     ::a/expression expr
     ::a/pos (pos-at i)
     ::a/candidates rs}
    (failure [i pat tail? env])))


;;; Adaptation.

(defmethod init 'ambiparse/scope [[i [_ pat] tail? env :as k]]
  (add-edge i pat tail? env k nil))

(defmethod passed 'ambiparse/scope [[i [_ pat] tail? env :as k] t]
  (pass-child k (assoc t ::a/env env)))

(defmethod -failure 'ambiparse/scope [[i [_ pat] tail? env]]
  (failure [i pat tail? env]))


;;; Execution.

(defn exec [[op k & args :as msg]]
  (log 'exec msg)
  (case op
    :init (init k)
    :pass (let [[t] args]
            (when-not (get-in graph (conj k :received t))
              (passed k t)
              (change! graph update-in (conj k :received) conjs t)))))

(defn pump []
  (log 'pump)
  ;; Execute queued messages.
  (let [q queue]
    (set! queue [])
    (doseq [msg q]
      (when (zero? (change! fuel dec))
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
            root [0 pat true {}]
            graph []
            queue []
            buffered #{}
            breaks (when (string? s) [0])
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
