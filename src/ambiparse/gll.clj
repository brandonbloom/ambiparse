(ns ambiparse.gll
  (:refer-clojure :exclude [send])
  (:require [clojure.spec :as s]
            [ambiparse.util :refer :all])
  (:import (java.util ArrayList HashSet)
           (ambiparse Machine)))

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

(def trace false)
;(def trace true)

(defmacro log [& xs]
  (require 'fipp.edn)
  (when trace
    `(fipp.edn/pprint (list ~@xs) {:width 200})))

;XXX (defn snapshot [^VM vm]
;XXX   {:input (.input vm)
;XXX    :root (.root vm)
;XXX    :graph (.graph vm)
;XXX    :queue (.queue vm)
;XXX    :fuel (.fuel vm)})

;;TODO: Better specs.
(s/def ::pattern any?)
(s/def ::tree any?)

(s/def ::key (s/spec (s/cat :i integer?, :pat ::pattern, :tail? boolean?)))

(defn scan-breaks [^Machine vm i]
  (when (< (.traveled vm) i)
    (doseq [n (range (.traveled vm) (min (inc i) (dec (count (.input vm)))))]
      (when (and (= (nth (.input vm) i) \newline)
                 (.breaks vm)
                 (< (peek @(.breaks vm)) n))
        (vswap! (.breaks vm) conj n)))
    (set! (.traveled vm) i)))

(defn input-at [^Machine vm i]
  (if (< i (count (.input vm)))
    (do (scan-breaks vm i)
        (nth (.input vm) i))
    ::a/eof))

(defn pos-at [^Machine vm i]
  (scan-breaks vm i)
  (if (.breaks vm)
    ;;TODO: Binary search?
    (reduce-kv (fn [res n b]
                 (if (<= i b)
                   (reduced res)
                   {:idx i :line (inc n) :col (- i b -1)}))
               {:idx i :line 1 :col (inc i)}
               @(.breaks vm))
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
  (fn [^Machine vm k] (dispatch k)))

(defmulti passed
  "Tells k about a successful sub-parse."
  (fn [^Machine vm k t] (dispatch k)))

(defmulti -failure
  "Asks k for a failure."
  (fn [^Machine vm k] (dispatch k)))

(def ^:dynamic inside)

(defn machine? [x]
  (instance? Machine x))

(s/fdef failure
  :args (s/alt :vm machine? :root (s/cat) :specific (s/cat :k ::key)))

(defn failure
  ([^Machine vm]
   (binding [inside #{}]
     (failure vm (.root vm))))
  ([^Machine vm [i _ _ :as k]]
   (when-not (inside k)
     (binding [inside (conj inside k)]
       (if-let [ex (get-in @(.graph vm) (conj k :exception))]
         {::a/exception ex ::a/pos (pos-at vm i)}
         (-failure vm k))))))

(defn received [^Machine vm k]
  (get-in @(.graph vm) (conj k :received)))

(def conjs (fnil conj #{}))

(defn send [^Machine vm msg]
  ;(log 'send msg)
  (vswap! (.queue vm) conj msg)
  nil)

(s/fdef add-node
  :args (s/cat :i integer?, :pat ::pattern, :tail? boolean?)
  :ret ::key)

(defn add-node [^Machine vm i pat tail?]
  (let [k [i pat tail?]]
    (when-not (get-in @(.graph vm) k)
      (vswap! (.graph vm) assoc-in k {})
      (send vm [:init k]))
    k))

(s/def ::prefix ::tree)
(s/def ::continue (s/nilable (s/coll-of ::pattern :kind seq?)))

(s/def ::decorator
  (s/keys :req-un [::prefix]
          :opt-un [::continue]))

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
  :args (s/cat :vm machine?
               :i integer?
               :pat any?
               :tail? boolean?
               :dst ::key
               :d (s/nilable ::decorator))
  :ret ::key)

(defn add-edge [^Machine vm i pat tail? dst d]
  (let [k (add-node vm i pat tail?)]
    (when-not (get-in @(.graph vm) (conj k :edges dst d))
      (vswap! (.graph vm) update-in (conj k :edges dst) conjs d)
      ;; Replay previously generated parses.
      (doseq [t (get-in @(.graph vm) (conj k :generated))]
        (send vm [:pass dst (decorate pat t d)])))
    k))

(s/fdef pass
  :args (s/cat :vm machine? :k ::key, :t ::tree))

(defn pass [^Machine vm [_ pat tail? :as k] t] ;XXX rename to "generate?" or "emit?"
  (when (or (not tail?)
            (= (-> t ::a/end :idx) (count (.input vm))))
    (let [t (assoc t ::a/pattern pat)]
      (when-not (get-in @(.graph vm) (conj k :generated t))
        (vswap! (.graph vm) update-in (conj k :generated) conjs t)
        (doseq [[dst ds] (get-in @(.graph vm) (conj k :edges))
                d ds]
          (send vm [:pass dst (decorate pat t d)]))))))

(defn pass-child [^Machine vm k t]
  (pass vm k (assoc t ::a/children [t])))

(defn empty-at [^Machine vm i]
  (let [p (pos-at vm i)]
    {::a/begin p
     ::a/end p
     ::a/children []
     ::a/elements []
     ::a/value []}))


;;; Terminals.

(defn lit-init [^Machine vm i c k]
  (let [x (input-at vm i)]
    (when (= x c)
      (pass vm k {::a/begin (pos-at vm i)
                  ::a/end (pos-at vm (inc i))
                  ::a/value x}))))

(defn lit-failure [^Machine vm i c]
  (let [x (input-at vm i)]
    (when (not= x c)
      {::a/pos (pos-at vm i)
       ::a/expected c
       ::a/actual x})))

(defmethod init 'ambiparse/lit [^Machine vm [i [_ c] _ :as k]]
  (lit-init vm i c k))

(defmethod -failure 'ambiparse/lit [^Machine vm [i [_ c] _]]
  (lit-failure vm i c))

(defmethod init java.lang.Character [^Machine vm [i c _ :as k]]
  (lit-init vm i c k))

(defmethod -failure java.lang.Character [^Machine vm [i c _]]
  (lit-failure vm i c))

(defmethod init java.lang.String [^Machine vm [i s _ :as k]]
  (loop [n 0]
    (if (< n (count s))
      (when (= (input-at vm (+ i n)) (nth s n))
        (recur (inc n)))
      (pass vm k {::a/begin (pos-at vm i)
                  ::a/end (pos-at vm (+ i n))
                  ::a/value s}))))

(defmethod -failure java.lang.String [^Machine vm [i s tail?]]
  (loop [n 0]
    (if (and (< n (count s)) (= (input-at vm (+ i n)) (nth s n)))
      (recur (inc n))
      (let [j (min (count (.input vm)) (+ i (count s)))
            actual (subs (.instr vm) i j)]
        {::a/pos (pos-at vm (+ i n))
         ::a/expected s
         ::a/actual actual}))))

(defmethod init 'ambiparse/-pred [^Machine vm [i [_ _ f] _ :as k]]
  (let [x (input-at vm i)]
    (when (f x)
      (pass vm k {::a/begin (pos-at vm i)
                  ::a/end (pos-at vm (inc i))
                  ::a/value x}))))

(defmethod -failure 'ambiparse/-pred [^Machine vm [i [_ expr f] _ :as k]]
  (let [x (input-at vm i)]
    (when-not (f x)
      {::a/message "Predicate failed"
       ::a/pos (pos-at vm i)
       ::a/predicate f
       ::a/expression expr
       ::a/actual x})))


;;; Concatenation.

(defn do-cat [^Machine vm t [_ _ tail? :as k] pats]
  (if-let [[p & ps] pats]
    (let [i (-> t ::a/end :idx)
          d {:prefix t :continue ps}
          tl? (and (empty? ps) tail?)]
      (add-edge vm i p tl? k d))
    (pass vm k (assoc t ::a/structure (second k)))))

(defmethod init 'ambiparse/cat [^Machine vm [i [_ & pats] tail? :as k]]
  (do-cat vm (empty-at vm i) k pats))

(defmethod passed 'ambiparse/cat [^Machine vm k t]
  (do-cat vm (dissoc t ::a/continue) k (::a/continue t)))

(defn rightmost [kw xs]
  ;XXX return _all_ rightmost, otherwise nested alts mask other alts.
  (when (seq xs)
    (apply max-key #(-> % kw :idx) xs)))

(defn rightmost-received [vm k]
  (rightmost ::a/end (received vm k)))

(defmethod -failure 'ambiparse/cat [^Machine vm [i [_ & pats] tail? :as k]]
  (if-let [t (rightmost-received vm k)]
    (when-let [cont (::a/continue t)]
      (failure vm [(-> t ::a/end :idx) (first cont) tail?]))
    (when-first [p pats]
      (failure vm [i p tail?]))))


;;; Alternation.

(defmethod init 'ambiparse/alt [^Machine vm [i [_ & pats] tail? :as k]]
  (doseq [pat pats]
    (add-edge vm i pat tail? k nil)))

(defmethod passed 'ambiparse/alt [^Machine vm k t]
  (pass-child vm k t))

(defmethod -failure 'ambiparse/alt [^Machine vm [i [_ & pats] tail?]]
  (let [errs (->> pats (map #(failure vm [i % tail?])) (remove nil?))
        pos (->> errs (rightmost ::a/pos) ::a/pos)
        errs (filter #(= (::a/pos %) pos) errs)]
    (cond
      (next errs) {::a/pos (pos-at vm i) ::a/alts (set errs)}
      (seq errs) (first errs))))


;;; Repetition.

(defn do-rep [^Machine vm [_ [_ pat] tail? :as k] t]
  (pass vm k t)
  (let [i (-> t ::a/end :idx)]
    (add-edge vm i pat false k {:prefix t})))

(defmethod init 'ambiparse/* [^Machine vm [i _ tail? :as k]]
  (do-rep vm k (empty-at vm i)))

(defmethod init 'ambiparse/+ [^Machine vm [i [_ pat] tail? :as k]]
  (add-edge vm i pat false k {:prefix (empty-at vm i)}))

(defmethod passed 'ambiparse/* [^Machine vm k t]
  (do-rep vm k t))

(defmethod passed 'ambiparse/+ [^Machine vm k t]
  (do-rep vm k t))

(defn rep-failure [^Machine vm [i [_ pat] tail? :as k]]
  (when tail?
    (let [e (or (-> (rightmost-received vm k) ::a/end :idx) i)]
      (failure vm [e pat false]))))

(defmethod -failure 'ambiparse/* [^Machine vm k]
  (rep-failure vm k))

(defmethod -failure 'ambiparse/+ [^Machine vm [i [_ pat] tail? :as k]]
  (or (failure vm [i pat false])
      (rep-failure vm k)))


;;; Optional.

(defmethod init 'ambiparse/? [^Machine vm [i [_ pat] tail? :as k]]
  (let [t (empty-at vm i)]
    (pass vm k t)
    (add-edge vm i pat tail? k {:prefix t})))

(defmethod passed 'ambiparse/? [^Machine vm k t]
  (pass vm k t))

(defmethod -failure 'ambiparse/? [^Machine vm [i [_ pat] tail?]]
  (when tail?
    (failure vm [i pat tail?])))


;;; Transformation.

(defmethod init 'ambiparse/-rule [^Machine vm [i [_ pat _ f] tail? :as k]]
  (add-edge vm i pat tail? k nil))

(defmethod passed 'ambiparse/-rule [^Machine vm [i [_ pat _ f] tail? :as k] t]
  (pass-child vm k (f t)))

(defmethod -failure 'ambiparse/-rule
  [^Machine vm [i [_ pat expr _] tail? :as k]]
  ;;XXX Use expr if the rule failed.
  (failure vm [i pat tail?]))


;;; Labeling.

(defmethod init 'ambiparse/label [^Machine vm [i [_ name pat] tail? :as k]]
  (add-edge vm i pat tail? k nil))

(defn strip-labels [t]
  (->> t (filter (fn [[k v]] (= (namespace k) "ambiparse"))) (into {})))

(defmethod passed 'ambiparse/label [^Machine vm [i [_ name pat] tail? :as k] t]
  (pass-child vm k (assoc (strip-labels t) name (::a/value t))))

(defmethod -failure 'ambiparse/label [^Machine vm [i [_ _ pat] tail?]]
  (failure vm [i pat tail?]))


;;; Indirection.

(defmethod init clojure.lang.Var [^Machine vm [i pat tail? :as k]]
  (add-edge vm i @pat tail? k nil))

(defmethod passed clojure.lang.Var [^Machine vm [i pat tail? :as k] t]
  (pass-child vm k (assoc t ::a/var pat)))

(defmethod -failure clojure.lang.Var [^Machine vm [i pat tail?]]
  (some-> (failure vm [i @pat tail?])
          (assoc ::a/var pat)))


;;; Precedence.

(defmethod init 'ambiparse/-prefer [^Machine vm [i [_ _ pat cmp] tail? :as k]]
  (add-edge vm i pat tail? k nil))

(defmethod passed 'ambiparse/-prefer
  [^Machine vm [i [_ _ pat cmp] tail? :as k] t]
  ;;XXX set exception if cmp fails.
  (let [buffer (get-in @(.graph vm) (conj k :buffer))
        buffer* (cond
                  ;; First parse.
                  (empty? buffer) #{t}
                  ;; Strictly preferrable.
                  (every? #(neg? (cmp % t)) buffer) #{t}
                  ;; Not strictly less preferrable.
                  ;;TODO: Compare in one pass over buffer.
                  (some #(zero? (cmp % t)) buffer) (conjs buffer t))]
    (when buffer*
      (vswap! (.graph vm) assoc-in (conj k :buffer) buffer*)
      (vswap! (.buffered vm) conj k))))

(defmethod -failure 'ambiparse/-prefer [^Machine vm [i [_ _ pat] tail?]]
  (failure vm [i pat tail?]))


;;; Filtering.
;;TODO: remove seems more common - better primative?

(defmethod init 'ambiparse/-filter [^Machine vm [i [_ _ pat _] tail? :as k]]
  (add-edge vm i pat tail? k nil))

(defmethod passed 'ambiparse/-filter
  [^Machine vm [i [_ _ pat f] tail? :as k] t]
  (when (f t)
    (pass-child vm k t)))

(defmethod -failure 'ambiparse/-filter
  [^Machine vm [i [_ expr pat f] tail? :as k]]
  (if-let [rs (received vm k)]
    {::a/message "Filter predicate failed"
     ::a/predicate f
     ::a/expression expr
     ::a/pos (pos-at vm i)
     ::a/candidates rs}
    (failure vm [i pat tail?])))


;;; Execution.

(defn exec [^Machine vm [op k & args :as msg]]
  (log 'exec msg)
  (try
    (case op
      :init (init vm k)
      :pass (let [[t] args]
              (when-not (get-in @(.graph vm) (conj k :received t))
                (passed vm k t)
                (vswap! (.graph vm) update-in (conj k :received) conjs t))))
    (catch Exception ex ;XXX
      (log 'catch-at k ex)
      (vswap! (.graph vm) update-in (conj k :exception) #(or % ex)))))

(defn pump [^Machine vm]
  (log 'pump)
  ;; Execute queued messages.
  (let [q @(.queue vm)]
    (vreset! (.queue vm) [])
    (doseq [msg q]
      (when (zero? (set! (.fuel vm) (dec (.fuel vm))))
        (throw (Exception. "out of fuel!")))
      (exec vm msg)))
  ;; When subgraphs quiesce, flush buffers.
  (when (empty? @(.queue vm))
    (log 'quiescence)
    (when-let [q (seq @(.buffered vm))]
      (vreset! (.buffered vm) #{})
      (doseq [k q
              :when (not (get-in @(.graph vm) (conj k :exception)))
              t (get-in @(.graph vm) (conj k :buffer))]
        (pass-child vm k t)
        ;;XXX clear the buffer!
        ))))

(defn indexed [x]
  (if (instance? clojure.lang.Indexed x)
    x
    (reify
      clojure.lang.Counted
      (count [_] (count x))
      clojure.lang.Indexed
      (nth [_ i] (nth x i))
      (nth [_ i notFound] (nth x i notFound)))))

(defn run [pat s]
  (let [vm (Machine.)]
    (set! (.input vm) (indexed s))
    (when (string? s)
      (set! (.instr vm) s))
    (set! (.root vm) [0 pat true])
    (set! (.graph vm) (volatile! []))
    (when (string? s)
      (set! (.breaks vm) (volatile! [0])))
    (set! (.queue vm) (volatile! []))
    (set! (.buffered vm) (volatile! #{}))
    ;(set! (.fuel vm) 400)
    (apply add-node vm (.root vm))
    (try
      (while (seq @(.queue vm))
        (pump vm))
      (finally
        (log 'final-state= (state))
        ;(ambiparse.viz/show! graph)
        ))
    vm))

(defn generated [^Machine vm k]
  (get-in @(.graph vm) (conj k :generated)))

(defn trees [^Machine vm]
  (generated vm (.root vm)))

(defn parses [^Machine vm]
  (map ::a/value (trees vm)))

(defn parse [^Machine vm]
  (let [ps (distinct (parses vm))]
    (if (seq ps)
      [(first ps) (when (next ps)
                    {::a/message "Ambiguous" ::a/parses (take 2 ps)})]
      [nil (or (failure vm) (throw (Exception. "Unknown failure")))])))

(defn parse! [^Machine vm]
  (let [[p err] (parse vm)]
    (when err
      (throw (ex-info "Parse failed" err)))
    p))
