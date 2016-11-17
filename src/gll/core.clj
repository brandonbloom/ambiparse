(ns gll.core
  (:refer-clojure :exclude [cat send * +])
  (:require [gll.util :refer :all]))

(defn cat [& pats]
  (list* `cat pats))

(defn alt [& pats]
  (list* `alt pats))

(defn * [pat]
  (list `* pat))

(defn + [pat]
  (list `+ pat))

(defn rule* [pat f]
  (list `rule* pat f))

(defmacro rule [pat & body]
  `(rule* ~pat
          (fn [{~'&begin :begin
                ~'&end :end
                ~'% :value
                :as t#}]
            (assoc t# :value (do ~@body)))))

;;; Glossary:
;;;   i = index in to source string
;;;   pat = pattern
;;;   k = key of node (pair of i and pat)
;;;   t = tree
;;;   c = char (or other atomic terminal)
;;;   s = string (ie. sequence of terminals)
;;;   src = source key of edge
;;;   dst = destination key of edge
;;;   d = decorator attached to edges

;(def trace true)
(def trace false)

(defmacro log [& xs]
  (require 'fipp.edn)
  (when trace
    `(fipp.edn/pprint (list ~@xs))))

(def ^:dynamic input)
(def ^:dynamic graph)
(def ^:dynamic queue)
(def ^:dynamic fuel)

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

(defmethod decorate :prefix [{x :value, :keys [end]}
                              [_ {xs :value, :keys [begin]}]]
  {:begin begin
   :value (conj xs x)
   :end end})

(defmethod decorate :single [t _]
  (update t :value vector))

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
        t {:begin {:idx i}
           :end {:idx (inc i)}
           :value x}
        f (if (= x c) pass fail)]
    (f k t)))

(defn pass-empty [[i & _ :as k]]
  (log 'pass-empty k)
  (pass k {:begin {:idx i}
           :end {:idx i}
           :value []}))

(defmethod init `cat [[i [_ & pats] :as k]]
  (if (seq pats)
    (let [cont (add-node i [:seq pats k])]
      (add-edge i (first pats) cont :single))
    (pass-empty k)))

(defmethod tell :root [k t]
  (log 'parsed! t)
  (when (= (-> t :end :idx) (count input))
    (pass k t)))

(defmethod tell `cat [k t]
  (pass k t))

(defmethod init :seq [k]
  nil)

(defmethod tell :seq [[i [_ pats dst]] t]
  (if (next pats)
    (let [e (-> t :end :idx)]
      (add-edge e (second pats)
                [e [:seq (next pats) dst]]
                [:prefix t]))
    (send [:tell dst t])))

(defmethod init `alt [[i [_ & pats] :as k]]
  (doseq [pat pats]
    (add-edge i pat k :identity)))

(defmethod tell `alt [k t]
  (pass k t))

(defmethod init `* [[i [_ pat] :as k]]
  (let [cont (add-node i [:rep pat k])]
    (add-edge i pat cont :single))
  (pass-empty k))

(defmethod init `+ [[i [_ pat] :as k]]
  (let [cont (add-node i [:rep pat k])]
    (add-edge i pat cont :single)))

(defmethod tell `* [k t]
  (pass k t))

(defmethod tell `+ [k t]
  (pass k t))

(defmethod init :rep [k]
  nil)

(defmethod tell :rep [[i [_ pat dst]] t]
  (let [e (-> t :end :idx)]
    (add-edge e pat
              [e [:rep pat dst]]
              [:prefix t]))
  (send [:tell dst t]))

(defmethod init `rule* [[i [_ pat f] :as k]]
  (add-edge i pat k :identity))

(defmethod tell `rule* [[i [_ pat f] :as k] t]
  (pass k (f t)))

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

(defn running? []
  (and (pos? fuel)
       (seq queue)))

(def root [0 :root])

(defn parses [pat s]
  (binding [input s
            graph {}
            queue []
            fuel 50] ;XXX Only enable when debugging.
    (add-edge 0 pat root :identity)
    (while (running?)
      (update! fuel dec)
      (pump))
    (log 'final-state= (state))
    (->> (get-in graph [0 :root :parses])
         (map :value))))

(defn parse [pat s]
  (let [ps (parses pat s)]
    (cond
      (next ps) (throw (ex-info "Ambiguous parse:" {:parses (take 2 ps)}))
      (seq ps) (first ps)
      :else (throw (ex-info "Parse failed" {:error "TODO"}))))) ;XXX

(comment

  (require 'fipp.edn)
  (fipp.edn/pprint
    ;(parses \x "x")
    ;(parses (cat) "")
    ;(parses (cat) "x")
    ;(parses (cat \x) "x")
    ;(parses (cat \x \y) "x")
    ;(parses (cat \x \y) "xy")
    ;(parses (cat \x \y \z) "xy")
    ;(parses (cat \x \y \z) "xyz")
    ;(parses (alt) "")
    ;(parses (alt \x) "x")
    ;(parses (alt \x \y) "x")
    ;(parses (alt \x \y) "y")
    ;(parses (alt \x \y) "z")
    ;(parses (* \x) "x")
    ;(parses (* \x) "xx")
    (parses (rule \x [&begin &end %]) "x")
    )

)
