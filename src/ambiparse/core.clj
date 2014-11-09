(ns ambiparse.core
  (:refer-clojure :exclude [derive empty concat]))

(defprotocol Parser
  (nullable? [this])
  (derive [this c])
  (form [this]))

(extend-protocol Parser
  clojure.lang.Var
  (nullable? [this] (nullable? @this))
  (derive [this c] (derive @this c))
  (form [this] this))

(defn direct [x]
  (if (var? x)
    (recur @x)
    x))

(def empty
  (reify Parser
    (nullable? [_] false)
    (derive [_ c] empty)
    (form [_] 'empty)))

(def epsilon
  (reify Parser
    (nullable? [_] true)
    (derive [_ c] empty)
    (form [_] 'epsilon)))

(defn literal [c]
  (reify Parser
    (nullable? [_] false)
    (derive [_ in-c] (if (= c in-c) epsilon empty))
    (form [_] (list 'literal c))))

(defn union [left right]
  (let [memo (atom {})
        null (atom nil)]
    (reify Parser
      (nullable? [_]
        (if-some [x @null]
          x
          (let [step #(reset! null (or (nullable? left) (nullable? right)))]
            (reset! null false)
            (reduce (fn [prev cur] (if (= prev cur) (reduced cur) cur))
                    (repeatedly step)))))
      (derive [_ c]
        (or (@memo c)
            (let [l (direct left), r (direct right)
                  ret (cond
                        (and (= l empty) (= r empty)) empty
                        (= l empty) (derive r c)
                        (= r empty) (derive l c)
                        :else (union (derive l c) (derive r c)))]
              (swap! memo assoc c ret)
              ret)))
      (form [this]
        (list 'union (form left) (form right))))))

(defn concat [left right]
  (if (= (direct right) empty)
    left
    (reify Parser
      (nullable? [_]
        (and (nullable? left) (nullable? right)))
      (derive [_ c]
        (if (nullable? left)
          (union (concat (derive left c) right)
                 (derive right c))
          (concat (derive left c) right)))
      (form [this]
        (list 'concat (form left) (form right))))))

(defn matches? [parser input]
  (if-let [[x & xs] (seq input)]
    (and (not= (direct parser) epsilon)
         (recur (derive parser x) xs))
    (nullable? parser)))

(comment

  (do
    (def A (union (concat (literal \a) #'A) epsilon))
    (def B (union (concat (literal \b) #'B) epsilon))
    (def AB (concat #'A #'B))
    (-> #'AB
        ;(derive \a)
        ;(derive \a)
        ;form
        ;nullable?
        (matches? "aaabbbbbbbbbbb")
        ))

)
