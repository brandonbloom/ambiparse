(ns ambiparse.viz
  (:require [dorothy.core :as d]
            [fipp.edn :refer [pprint]]))

(defn edge-label [x]
  (with-out-str
    (-> x
        (update-in [:prefix :ambiparse/begin] :idx)
        (update-in [:prefix :ambiparse/end] :idx)
        (pprint {:width 20}))))

(defn identify [ids k]
  (or (@ids k)
      (let [id (inc (count @ids))]
        (swap! ids assoc k id)
        id)))

(defn to-dorothy [g]
  (let [ids (atom {})]
     (for [[i nodes] (map vector (range) g)
           [pat {:keys [edges]}] nodes
           :let [k [i pat]
                 src-id (identify ids k)]]
       ;; Nodes.
       (list [src-id {:label (pr-str k)}]
             ;; Edges.
             (for [[dst decorators] edges
                   :let [dst-id (identify ids dst)]
                   decorator decorators]
               [src-id dst-id {:label (edge-label decorator)}])))))

(defn show! [g]
  (-> g to-dorothy d/digraph d/dot (doto println) d/show!))
