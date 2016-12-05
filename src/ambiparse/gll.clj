(ns ambiparse.gll
  (:refer-clojure :exclude [send])
  (:require [clojure.spec :as s]
            [ambiparse.util :refer :all]
            [ambiparse.viz :as viz]))

(create-ns 'ambiparse)
(alias 'a 'ambiparse)

;;; Glossary:
;;;   i = index in to source string
;;;   b = begin index of span
;;;   e = end index of span
;;;   pat = pattern
;;;   k = key of node (pair of i and pat)
;;;   n = node
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
(def ^{:dynamic true
       :doc "Steps to perform before giving up. Set to 0 to disable."}
  fuel)

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

(defrecord Context [^long i, ^boolean tail?, env])

(defn context? [x]
  (instance? Context x))

(def root-ctx (Context. 0 true {}))

(defrecord Key [pat, ^Context ctx])

(defn key? [x]
  (instance? Key x))

(s/def ::token any?)
(s/def ::pos (s/keys :req-un [::idx] :opt-un [::line ::col ::token]))
(s/def ::line int?)
(s/def ::col int?)
(s/def ::idx int?)

(s/def ::env (s/every-kv var? (s/every ::pat, :kind set?)))

(s/def ::pattern some?)

(s/def ::span (s/cat :begin ::idx :end ::idx))

(s/def ::a/begin ::pos)
(s/def ::a/end ::pos)
(s/def ::a/children (s/every ::tree :kind vector?))
(s/def ::a/pattern ::pattern)
(s/def ::a/matched (s/every-kv ::pattern (s/every ::span :kind set?)))
(s/def ::a/structure ::pattern)
(s/def ::a/elements (s/every ::tree :kind vector?))
(s/def ::a/env ::env)
(s/def ::a/continue (s/every ::pattern))

(s/def ::passed
  (s/keys :req [::a/begin ::a/end ::a/value ::a/env]
          :opt [::a/children ::a/structure]))

(s/def ::tree
  (s/merge ::passed
           (s/keys :req [::a/pattern]
                   :opt [::a/elements ::a/continue ::a/matched])))

(defn scan-breaks [^long i]
  (let [lt (long traveled)]
    (when (< lt i)
      (doseq [^long n (range lt (min (inc i) (dec (count input))))]
        (when (and (= (nth input i) \newline)
                   breaks
                   (< (peek breaks) n))
          (change! breaks conj n)))
      (set! traveled i))))

(defn input-at [^long i]
  (if (< i (count input))
    (do (scan-breaks i)
        (nth input i))
    ::a/eof))

(defn pos-at [^long i]
  (scan-breaks i)
  (if breaks
    ;;TODO: Binary search?
    (reduce-kv (fn [res, ^long n, ^long b]
                 (if (<= i b)
                   (reduced res)
                   {:idx i :line (inc n) :col (- i b -1)}))
               {:idx i :line 1 :col (inc i)}
               breaks)
    {:idx i :token (input-at i)}))

(defn node-path [^Key k]
  (let [^Context ctx (.ctx k)
        i (.i ctx)]
    [i k]))

(defn get-node [^Key k]
  (get-in graph (node-path k)))

(defn rightmost [kw xs]
  ;XXX return _all_ rightmost, otherwise nested alts mask other alts.
  (when (seq xs)
    (apply max-key #(-> % kw :idx) xs)))

(defn rightmost-received [k]
  (rightmost ::a/end (-> k get-node :received)))

(defn classify [pat]
  (cond
    (sequential? pat) (first pat)
    (= pat 'ambiparse/eof) 'ambiparse/eof
    :else (class pat)))

;; Fully re-create multimethods for dev sanity.
(doseq [sym '[init passed -failure]]
  (ns-unmap *ns* sym))

;; For each of these, (= (Key. pat ctx) k)
;; They are separated out for ease of type hinting and field access.

(defmulti init
  "Called when a parse node for a given key is first created."
  (fn [pat ctx k]
    (classify pat)))

(defmulti passed
  "Tells actor at (Key. pat ctx) about a successful sub-parse."
  (fn [pat ctx k t]
    (classify pat)))

(defmulti -failure
  "Asks actor at (Key. pat ctx) for a failure."
  (fn [pat ctx k]
    (classify pat)))

(def ^:dynamic inside)

;;TODO: Spec failure objects.
;;TODO: Extend failure objects to include both begin and end instead of just
;; pos. Begin is where to report error, end how far parser got for sake of
;; finding the "best" error.
(s/fdef failure :args (s/alt :root (s/cat) :specific (s/cat :k key?)))

(defn failure
  ([]
   (binding [inside #{}]
     (failure root)))
  ([^Key k]
   (let [^Context ctx (.ctx k)
         n (get-node k)]
     (when-not (inside k)
       (binding [inside (conj inside k)]
         (if-let [ex (:exception n)]
           (let [pos (pos-at (or (-> (rightmost-received k) ::a/end :idx)
                                 (.i ctx)))]
             {::a/exception ex ::a/pos pos})
           (-failure (.pat k) ctx k)))))))

(defn send [msg]
  ;(log 'send msg)
  (change! queue conj msg)
  nil)

(s/fdef add-node
  :args (s/cat :pat ::pattern, :ctx context?))

(defn head-fail [i pat]
  (cond
    (char? pat) (not= (input-at i) pat)
    (= pat 'ambiparse/eof) (not= i (count input))
    :else (when-let [f (-> pat meta ::a/head-fail)]
            (f (input-at i)))))

(defn add-node [pat, ^Context ctx]
  (when-not (head-fail (.i ctx) pat)
    (let [k (Key. pat ctx)]
      (when-not (get-node k)
        (change! graph assoc-in (node-path k) {})
        (send [:init k]))
      k)))

(s/def ::prefix ::passed)
(s/def ::continue (s/nilable (s/every ::pattern :kind seq?)))

(s/def ::decorator
  (s/keys :req-un [::prefix]
          :opt-un [::continue]))

(s/fdef decorate
  :args (s/cat :t ::tree, ::d (s/nilable ::decorator))
  :ret ::tree)

(defn decorate
  "Applies a transformation to trees flowing along an edge."
  [t {:as d :keys [prefix continue]}]
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
  :args (s/cat :pat some?
               :ctx context?
               :dst key?
               :d (s/nilable ::decorator)))

(defn add-edge [pat ctx dst d]
  (when-let [k (add-node pat ctx)]
    (let [n (get-node k)]
      (when-not (get-in n [:edges dst d])
        (change! graph update-in (conj (node-path k) :edges dst) conjs d)
        ;; Replay previously generated parses.
        (doseq [t (:generated n)]
          (send [:pass dst (decorate t d)]))))))

(s/fdef pass
  :args (s/cat :k key?, :t ::passed))

(defn pass [{:keys [pat ctx] :as k} t]
  ;;XXX forward ::a/matched
  (when (or (not (.tail? ^Context ctx))
            (= (-> t ::a/end :idx) (count input)))
    (let [v (::a/value t)
          t (assoc t ::a/pattern pat)
          n (get-node k)]
      (when-not (get-in n [:generated t])
        (change! graph update-in (conj (node-path k) :generated) conjs t)
        (doseq [[dst ds] (:edges n)
                d ds]
          (send [:pass dst (decorate t d)]))))))

(defn pass-child [k t]
  (pass k (assoc t ::a/children [t])))

(defn empty-in [^Context ctx]
  (let [p (pos-at (.i ctx))]
    {::a/begin p
     ::a/end p
     ::a/children []
     ::a/elements []
     ::a/value []
     ::a/env (.env ctx)}))

(defn report-ex [k ex]
  (log 'catch-at k ex)
  (change! graph update-in (conj (node-path k) :exception) #(or % ex))
  nil)

(defmacro try-at [k & body]
  `(let [k# ~k]
     (try
       ~@body
       (catch ~'Exception ex#
         (report-ex k# ex#)))))

(defn report-ex [k ex]
  (log 'catch-at k ex)
  (change! graph update-in (conj (node-path k) :exception) #(or % ex))
  nil)

(defmacro try-at [k & body]
  `(let [k# ~k]
     (try
       ~@body
       (catch ~'Exception ex#
         (report-ex k# ex#)))))


;;; Terminals.

(defn lit-init [^long i, c, {:keys [ctx] :as k}]
  (let [x (input-at i)]
    (when (= x c)
      (pass k {::a/begin (pos-at i)
               ::a/end (pos-at (inc i))
               ::a/value x
               ::a/env (.env ^Context ctx)}))))

(defn lit-failure [^long i, c]
  (let [x (input-at i)]
    (when (not= x c)
      {::a/pos (pos-at i)
       ::a/expected c
       ::a/actual x})))

(defmethod init 'ambiparse/lit [[_ c], ^Context ctx, k]
  (lit-init (.i ctx) c k))

(defmethod -failure 'ambiparse/lit [[_ c], ^Context ctx, k]
  (lit-failure (.i ctx) c))

(defmethod init java.lang.Character [c, ^Context ctx, k]
  (lit-init (.i ctx) c k))

(defmethod -failure java.lang.Character [c, ^Context ctx, k]
  (lit-failure (.i ctx) c))

(defmethod init java.lang.String [s, ^Context ctx, k]
  (let [i (.i ctx)]
    (loop [n 0]
      (if (< n (count s))
        (when (= (input-at (+ i n)) (nth s n))
          (recur (inc n)))
        (pass k {::a/begin (pos-at i)
                 ::a/end (pos-at (+ i n))
                 ::a/value s
                 ::a/env (.env ctx)})))))

(defmethod -failure java.lang.String [s, ^Context ctx, k]
  (let [i (.i ctx)]
    (loop [n 0]
      (if (and (< n (count s)) (= (input-at (+ i n)) (nth s n)))
        (recur (inc n))
        (let [i (.i ctx)
              actual (subs input i (min (count input) (+ i (count s))))]
          {::a/pos (pos-at (+ i n))
           ::a/expected s
           ::a/actual actual})))))

(defmethod init 'ambiparse/-pred [[_ _ f], ^Context ctx, k]
  (let [i (.i ctx)
        x (input-at i)]
    (when (try-at k (f x))
      (pass k {::a/begin (pos-at i)
               ::a/end (pos-at (inc i))
               ::a/value x
               ::a/env (.env ctx)}))))

(defmethod -failure 'ambiparse/-pred [[_ expr f], ^Context ctx, k]
  (let [i (.i ctx)
        x (input-at i)]
    (when-not (f x)
      {::a/message "Predicate failed"
       ::a/pos (pos-at i)
       ::a/predicate f
       ::a/expression expr
       ::a/actual x})))

(defmethod init 'ambiparse/eof [_, ^Context ctx, k]
  (let [i (.i ctx)]
    (when (= (count input) i)
      (let [pos (pos-at i)]
        (pass k {::a/begin pos
                 ::a/end pos
                 ::a/value ::a/eof
                 ::a/env (.env ctx)})))))

(defmethod -failure 'ambiparse/eof [_, ^Context ctx, k]
  (let [i (.i ctx)]
    {::a/pos (pos-at i)
     ::a/expected ::a/eof
     ::a/actual (input-at i)}))


;;; Concatenation.

(defn do-cat [t, ^Context ctx, ^Key k, pats]
  (if-let [[p & ps] pats]
    (let [i (-> t ::a/end :idx)
          tail? (and (empty? ps) (.tail? ctx))
          env (::a/env t)
          d {:prefix t :continue ps}]
      (add-edge p (Context. i tail? env) k d))
    (pass k (assoc t ::a/structure (.pat k)))))

(defmethod init 'ambiparse/cat [[_ & pats], ^Context ctx, k]
  (do-cat (empty-in ctx) ctx k pats))

(defmethod passed 'ambiparse/cat [_ ctx k t]
  (do-cat (dissoc t ::a/continue) ctx k (::a/continue t)))

(defmethod -failure 'ambiparse/cat
  [[_ & pats], ^Context ctx, k]
  (if-let [t (rightmost-received k)]
    (when-let [[pat & pats] (-> t ::a/continue seq)]
      (let [i (-> t ::a/end :idx)
            tail? (and (.tail? ctx) (empty? pats))
            env (::a/env t)
            ctx (Context. i tail? env)]
        (prn (Key. pat ctx))
        (failure (Key. pat ctx))))
    (when-first [pat pats]
      (failure (Key. pat ctx)))))


;;; Alternation.

(defn init-alt [^Key k, pats]
  (doseq [pat pats]
    (add-edge pat (.ctx k) k nil)))

(defmethod init 'ambiparse/alt
  [[_ & pats] _ k]
  (init-alt k pats))

(defmethod passed 'ambiparse/alt [_ _ k t]
  (pass-child k t))

(defn alt-failure
  [^Context ctx, pats]
  (let [errs (->> pats (map #(failure (Key. % ctx))) (remove nil?))
        pos (->> errs (rightmost ::a/pos) ::a/pos)
        errs (filter #(= (::a/pos %) pos) errs)]
    (cond
      (next errs) {::a/pos (pos-at (.i ctx)) ::a/alts (set errs)}
      (seq errs) (first errs))))

(defmethod -failure 'ambiparse/alt [[_ & pats] ctx k]
  (alt-failure ctx pats))


;;; Repetition.

(defn non-rec [pat t]
  (let [b (-> t ::a/begin :idx)
        e (-> t ::a/end :idx)
        span [b e]]
    (when-not (get-in t [::a/matched pat span])
      (update-in t [::a/matched pat] conjs span))))

(defn do-rep [[_ pat :as p] ctx k t]
  (pass k t)
  (when-let [t (non-rec pat t)]
    (let [b (-> t ::a/begin :idx)
          e (-> t ::a/end :idx)]
      (when (< b e)
        (add-edge pat (Context. e false (::a/env t)) k {:prefix t})))))

(defmethod init 'ambiparse/* [[_ pat] ctx k]
  (let [t (empty-in ctx)]
    (pass k t)
    (add-edge pat (assoc ctx :tail? false) k {:prefix t})))

(defmethod init 'ambiparse/+ [[_ pat] ctx k]
  (add-edge pat (assoc ctx :tail? false) k {:prefix (empty-in ctx)}))

(defmethod passed 'ambiparse/* [pat ctx k t]
  (do-rep pat ctx k t))

(defmethod passed 'ambiparse/+ [pat ctx k t]
  (do-rep pat ctx k t))

(defn rep-failure [[_ pat], ^Context ctx, k]
  (let [[e env] (if-let [t (rightmost-received k)]
                  [(-> t ::a/end :idx) (::a/env t)]
                  [(.i ctx) (.env ctx)])
        ctx* (Context. e false env)]
    (failure (Key. pat ctx*))))

(defmethod -failure 'ambiparse/* [pat ctx k]
  (rep-failure pat ctx k))

(defmethod -failure 'ambiparse/+ [[_ pat] ctx k]
  (or (failure (Key. pat (assoc ctx :tail? false)))
      (rep-failure pat ctx k)))


;;; Optional.

(defmethod init 'ambiparse/? [[_ pat] ctx k]
  (let [t (assoc (empty-in ctx) ::a/value nil)]
    (pass k t)
    (add-edge pat ctx k nil)))

(defmethod passed 'ambiparse/? [pat ctx k t]
  (pass k t))

(defmethod -failure 'ambiparse/? [[_ pat], ^Context ctx, k]
  (failure (Key. pat ctx)))


;;; Transformation.

(def ^:dynamic muts)

(defn modify [env]
  (reduce (fn [env [op v pat]]
            (case op
              :add (update env v conjs pat)
              :del (update env v disj pat)))
          env
          muts))

(defmethod init 'ambiparse/-rule
  [[_ pat _ f] ctx k]
  (add-edge pat ctx k nil))

(defmethod passed 'ambiparse/-rule
  [[_ pat _ f], ^Context ctx, k, t]
  (binding [muts []]
    (when-let [t* (try-at k (f t))]
      (let [t* (assoc t* ::a/env (modify (.env ctx)))]
        (pass-child k t*)))))

(defmethod -failure 'ambiparse/-rule
  [[_ pat expr _] ctx k]
  (failure (Key. pat ctx)))


;;; Labeling.

(defmethod init 'ambiparse/label
  [[_ _ pat] ctx k]
  (add-edge pat ctx k nil))

(defn strip-labels [t]
  (->> t (filter (fn [[k v]] (= (namespace k) "ambiparse"))) (into {})))

(defmethod passed 'ambiparse/label
  [[_ name pat] ctx k t]
  (pass-child k (assoc (strip-labels t) name (::a/value t))))

(defmethod -failure 'ambiparse/label
  [[_ _ pat] ctx k]
  (failure (Key. pat ctx)))


;;; Indirection.

(defn var-alts [var, ^Context ctx]
  (conj ((.env ctx) var #{}) @var))

(defmethod init clojure.lang.Var
  [pat ctx k]
  (init-alt k (var-alts pat ctx)))

(defmethod passed clojure.lang.Var
  [pat ctx k t]
  (when-let [t (non-rec pat t)]
    (pass-child k t)))

(defmethod -failure clojure.lang.Var
  [pat ctx k]
  (some-> (alt-failure ctx (var-alts pat ctx))
          (assoc ::a/var pat)))


;;; Precedence.

(defmethod init 'ambiparse/-prefer
  [[_ _ pat cmp] ctx k]
  (add-edge pat ctx k nil))

(defmethod passed 'ambiparse/-prefer
  [[_ _ pat cmp] ctx k t]
  (let [buffer (:buffer (get-node k))
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
      (change! graph assoc-in (conj (node-path k) :buffer) buffer*)
      (change! buffered conj k))))

(defmethod -failure 'ambiparse/-prefer
  [[_ _ pat] ctx k]
  (failure (Key. pat ctx)))


;;; Filtering.
;;TODO: remove seems more common - better primative?

(defmethod init 'ambiparse/-filter
  [[_ _ pat _] ctx k]
  (add-edge pat ctx k nil))

(defmethod passed 'ambiparse/-filter
  [[_ _ _ f] ctx k t]
  (when (try-at k (f t))
    (pass-child k t)))

(defmethod -failure 'ambiparse/-filter
  [[_ expr pat f], ^Context ctx, k]
  (if-let [rs (-> k get-node :received)]
    {::a/message "Filter predicate failed"
     ::a/predicate f
     ::a/expression expr
     ::a/pos (pos-at (.i ctx))
     ::a/candidates rs}
    (failure (Key. pat ctx))))


;;; Adaptation.

(defmethod init 'ambiparse/scope
  [[_ pat] ctx k]
  (add-edge pat ctx k nil))

(defmethod passed 'ambiparse/scope
  [[_ pat], ^Context ctx, k, t]
  (pass-child k (assoc t ::a/env (.env ctx))))

(defmethod -failure 'ambiparse/scope
  [[_ pat] ctx k]
  (failure (Key. pat ctx)))


;;; Execution.

(defn exec [[op ^Key k & args :as msg]]
  (log 'exec msg)
  (case op
    :init (init (.pat k) (.ctx k) k)
    :pass (let [[{v ::a/value, :as t}] args
                n (get-node k)]
            (when-not (get-in n [:received t])
              (passed (.pat k) (.ctx k) k t)
              (change! graph update-in (conj (node-path k) :received)
                       conjs t)))))

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
              :let [n (get-node k)]
              :when (not (:exception n))
              t (:buffer n)]
        (pass-child k t)
        ;;XXX clear the buffer!
        ))))

(defn run []
  (let [{:keys [pat ctx]} root]
    (add-node pat ctx))
  (while (seq queue)
    (pump)))

(defn with-run-fn [pat s opts f]
  (binding [input s
            root (Key. pat root-ctx)
            graph []
            queue []
            buffered #{}
            breaks (when (string? s) [0])
            traveled 0
            fuel (opts :fuel 0)]
    (try
      (run)
      (finally
        (log 'final-state= (state))
        (when (:viz opts)
          (viz/show! (state)))))
    (f)))

(defmacro with-run [pat s opts & body]
  `(with-run-fn ~pat ~s ~opts (fn [] ~@body)))

(defn trees []
  (-> root get-node :generated))

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
