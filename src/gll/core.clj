(ns gll.core
  (:refer-clojure :exclude [cat send])
  (:require [gll.util :refer :all]))

(defn cat [& pats]
  (list* `cat pats))

(defn alt [& pats]
  (list* `alt pats))

(defn rep [x]
  (list `rep x))

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

(doseq [sym '[init transform tell]]
  (ns-unmap *ns* sym))
(defmulti init dispatch)
(defmulti transform (fn [t by] (classify by)))
(defmulti tell (fn [k t] (dispatch k)))

(def conjs (fnil conj #{}))

(defn send [msg]
  (prn (list 'send msg))
  (update! queue conj msg)
  nil)

(defn add-node [i pat]
  (let [k [i pat]]
    (when-not (get-in graph k)
      (send [:init k]))
    k))

(defn add-edge [i pat dst label]
  (let [k (add-node i pat)]
    (when-not (get-in graph (conj k :edges dst label))
      (update! graph update-in (conj k :edges dst) conjs label)
      (send [:link k dst label]))
    k))

(defmethod transform :identity [t _]
  t)

(defmethod transform :prefix [{x :value, :keys [end]}
                              [_ {xs :value, :keys [begin]}]]
  {:begin begin
   :value (conj xs x)
   :end end})

(defmethod transform :single [t _]
  (update t :value vector))

(defn pass [k t]
  (when-not (get-in graph (conj k :parses t))
    (update! graph update-in (conj k :parses) conjs t)
    (doseq [[dst labels] (get-in graph (conj k :edges))
            label labels]
      (send [:tell dst (transform t label)]))))

(defn fail [k t]
  (prn 'fail k t))

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

(defmethod init `cat [[i [_ & pats] :as k]]
  (if (seq pats)
    (let [cont (add-node i [:seq pats k])]
      (add-edge i (first pats) cont :single))
    (pass k {:begin {:idx i}
             :end {:idx i}
             :value []})))

(defmethod tell :root [k t]
  (prn 'parsed! t))

(defmethod tell `cat [k t]
  (pass k t))

(defmethod init :seq [[i [_ pats dst] :as k]]
  nil)

(defmethod tell :seq [[i [_ pats dst]] t]
  (prn 'tell-seq-dst dst)
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

(defn exec [[op & _ :as msg]]
  (println)
  (prn (list 'exec msg))
  (case op
    :init (let [[_ k] msg]
            (init k))
    :link (let [[_ src dst label] msg]
            (doseq [t (get-in graph (conj src :parses))]
              (send [:tell dst (transform t label)])))
    :tell (let [[_ k t] msg]
            (tell k t))
    ))

(defn pump []
  (println "\n\npump")
  (let [q queue]
    (set! queue [])
    (run! exec q)))

(defn running? []
  (and (pos? fuel)
       (seq queue)))

(def root [0 :root])

(defn parse [n s]
  (binding [input s
            graph {}
            queue []
            fuel 10]
    (add-edge 0 n root :identity)
    (while (running?)
      (update! fuel dec)
      (pump))
    (state)
    ))

(comment

  (require 'fipp.edn)
  (fipp.edn/pprint
    ;(parse \x "x")
    ;(parse (cat) "")
    ;(parse (cat) "x")
    ;(parse (cat \x) "x")
    ;(parse (cat \x \y) "x")
    ;(parse (cat \x \y) "xy")
    ;(parse (cat \x \y \z) "xy")
    ;(parse (cat \x \y \z) "xyz")
    ;(parse (alt) "")
    ;(parse (alt \x) "x")
    ;(parse (alt \x \y) "x")
    ;(parse (alt \x \y) "y")
    (parse (alt \x \y) "z")
    )

)
