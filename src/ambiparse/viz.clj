(ns ambiparse.viz
  (:require [dorothy.core :as d]
            [fipp.edn :refer [pprint]]))

(alias 'a 'ambiparse)

(defn pps [x]
  (with-out-str (pprint x {:width 20})))

(defn edge-label [x]
  (if x
    (str (-> x :prefix ::a/begin :idx) " - " (-> x :prefix ::a/end :idx) "\n"
         "pre: " (-> x :prefix ::a/value pps)
         (when-let [cont (:continue x)]
           (str "cont: " (pps cont))))
    ""))

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
  (-> g to-dorothy d/digraph d/dot d/show!))
