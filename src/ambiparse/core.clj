(ns ambiparse.core
  (:refer-clojure :exclude [derive])
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :refer [cartesian-product]]))


(defn lit [x]
  {:head :lit :value x})

(defn cat [& xs]
  {:head :cat :elements (vec xs)})

(defn alt [& xs]
  {:head :alt :alternatives (set xs)})

(defn prod [x]
  (assert (var? x))
  {:head :prod :var x})

(defn red [tag lang]
  {:head :red :tag tag :lang lang})

(def null (cat))
(def none (alt))


(defmacro defprod [name node]
  `(do
     (def ~name (prod (var ~name)))
     (alter-meta! (var ~name) assoc ::lang (expand ~node))
     (var ~name)))

(defn prod->red [prod]
  (assert (= (:head prod) :prod))
  (let [tag (let [m (-> prod :var meta)]
              (keyword (-> m :ns str) (-> m :name str)))
        lang (-> prod :var meta ::lang)]
    (red tag lang)))


(defn parsed [trees]
  (assert (set? trees))
  {:head :parsed :trees trees})


(defmulti expand1 class)

(defmethod expand1 java.lang.Character [x]
  (lit x))

(defmethod expand1 clojure.lang.IPersistentMap [x]
  x)

(defmethod expand1 clojure.lang.IPersistentVector [x]
  {:head :cat :elements x})

(defmethod expand1 String [x]
  {:head :cat :elements (vec x)})

(defmethod expand1 clojure.lang.IPersistentSet [x]
  {:head :alt :alternatives x})

(defmethod expand1 clojure.lang.Var [x]
  (prod x))


(defmulti expand (fn [x] (:head x)))

(defmethod expand nil [x] (expand (expand1 x)))

(defmethod expand :lit [lang] lang)

(defmethod expand :alt [lang]
  (update-in lang [:alternatives] #(set (map expand %))))

(defmethod expand :cat [lang]
  (update-in lang [:elements] #(mapv expand %)))

(defmethod expand :prod [lang]
  lang)



(def ^:dynamic *cache*)

(defmacro with-cache [& body]
  `(binding [*cache* {}]
     ~@body))

(defn memo [key thunk]
  (if-some [ret (*cache* key)]
    ret
    (let [ret (thunk)]
      (set! *cache* (assoc *cache* key ret))
      ret)))

(defn fix [key init step]
  (if-some [ret (*cache* key)]
    ret
    (let [step #(let [x (step)]
                  (set! *cache* (assoc *cache* key (step)))
                  x)]
      (set! *cache* (assoc *cache* key init))
      (reduce (fn [prev cur] (if (= prev cur) (reduced cur) cur))
              (repeatedly step)))))


(defmulti nullable? :head)

(defmethod nullable? :lit [node] false)

(defmethod nullable? :parsed [node] true)

(defmethod nullable? :cat [{:keys [elements]}]
  (empty? elements))

(defmethod nullable? :alt
  [{:keys [alternatives]}]
  (boolean (some nullable? alternatives)))

(defmethod nullable? :red
  [{:keys [lang]}]
  (nullable? lang))

(defmethod nullable? :prod
  [{:keys [var] :as prod}]
  (fix [:nullable? var]
       false
       (let [lang (prod->red prod)]
         #(nullable? lang))))


(defmulti null? :head)

(defmethod null? :lit [node] false)

(defmethod null? :parsed [node] false)

(defmethod null? :cat [{:keys [elements]}]
  (every? null? elements))

(defmethod null? :alt
  [{:keys [alternatives] :as lang}]
  (and (not= lang none)
       (every? null? alternatives)))

(defmethod null? :red
  [{:keys [lang]}]
  (null? lang))

(defmethod null? :prod
  [{:keys [var] :as prod}]
  (fix [:null? var]
       true
       (let [lang (prod->red prod)]
         #(null? lang))))


(defmulti none? :head)

(defmethod none? :lit [node] false)

(defmethod none? :parsed [node] false)

(defmethod none? :cat
  [{:keys [elements]}]
  (boolean (some none? elements)))

(defmethod none? :alt
  [{:keys [alternatives]}]
  (every? none? alternatives))

(defmethod none? :red
  [{:keys [lang]}]
  (none? lang))

(defmethod none? :prod
  [{:keys [var] :as prod}]
  (fix [:none? var]
       true
       (let [lang (prod->red prod)]
         #(none? lang))))


(defmulti parses :head)

(defmethod parses :parsed [{:keys [trees]}] trees)

(defmethod parses :lit [_] #{})

(->> '(#{[\a]} #{[\x] [\y]})
     (apply cartesian-product)
     (map #(apply concat %))
     )

(defmethod parses :cat [{:keys [elements]}]
  (->> (map parses elements)
       (apply cartesian-product)
       (map #(apply concat %))
       set))

(defmethod parses :alt [{:keys [alternatives]}]
  (apply set/union (map parses alternatives)))

(defmethod parses :red [{:keys [tag lang]}]
  (set (for [p (parses lang)]
         [(into [tag] p)])))

(defmethod parses :prod [x]
  (fix [:parses x]
       #{}
       #(parses (prod->red x))))


(def ^:dynamic *simplifying* #{})

(defmulti simplify :head)

(defmethod simplify :lit [x] x)
(defmethod simplify :parsed [x] x)

(defmethod simplify :cat
  [{:keys [elements]}]
  (let [elms (->> elements
                  (map simplify)
                  (mapcat #(or (:elements %) [%]))
                  (remove null?))]
    (case (count (take 2 elms))
      0 null
      1 (first elms)
      (apply cat elms))))

(defmethod simplify :alt
  [{:keys [alternatives]}]
  (let [alts (->> alternatives
                  (map simplify)
                  (mapcat #(or (:alternatives %) #{%}))
                  (remove none?))]
    (case (count (take 2 alts))
      0 none
      1 (first alts)
      (apply alt alts))))

(defmethod simplify :red
  [{:keys [tag lang]}]
  (red tag (simplify lang)))

(defmethod simplify :prod
  [prod]
  (let [lang (prod->red prod)]
    (if (*simplifying* prod)
      lang
      (binding [*simplifying* (conj *simplifying* prod)]
        (memo [:simplify prod]
              #(simplify lang))))))



;; Assumes language has been pre-simplified.
(defmulti derive (fn [{:keys [head]} x] head))

(defmethod derive :parsed [x c] none)

(defmethod derive :lit
  [{:keys [value]} x]
  (if (= value x)
    (parsed #{[x]})
    none))

(defmethod derive :cat
  [{:keys [elements]} x]
  ;;TODO can this be simpler?
  (if (seq elements)
    (let [[y & ys] elements]
      (if (nullable? y)
        (alt (cat (parsed (parses y)) (derive (apply cat ys) x))
             (apply cat (derive y x) ys))
        (apply cat (derive y x) ys)))
    none))

(defmethod derive :alt
  [{:keys [alternatives]} x]
  (apply alt (map #(derive % x) alternatives)))

(defmethod derive :red
  [{:keys [tag lang]} x]
  (red tag (derive lang x)))

(defmethod derive :prod
  [prod x]
  (memo [:derive prod]
        #(derive (prod->red prod) x)))



(defn parse [lang input]
  (with-cache
    (loop [parser (expand lang)
           xs (seq input)]
      (if-let [[x & xs] xs]
        (recur (simplify (derive parser x)) xs)
        (parses parser)))))
