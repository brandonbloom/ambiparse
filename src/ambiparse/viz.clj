(ns ambiparse.viz
  (:require [dorothy.core :as d]
            [fipp.edn :refer [pprint]]))

(create-ns 'ambiparse)
(alias 'a 'ambiparse)

(defn pps [x]
  (with-out-str (pprint x {:width 30})))

;;TODO: Cleanup and expose this?
(defn unform [pat]
  (if (sequential? pat)
    (case (first pat)
      ambiparse/lit (list* 'a/lit (second pat))
      ambiparse/-pred (list 'a/pred (second pat))
      ambiparse/cat (list* 'a/cat (map unform (next pat)))
      ambiparse/alt (list* 'a/alt (map unform (next pat)))
      ambiparse/label (list 'a/label (second pat) (-> pat (nth 2) unform))
      ambiparse/-rule (list 'a/rule (-> pat second unform) (nth pat 2))
      ambiparse/+ (list 'a/+ (-> pat second unform))
      ambiparse/* (list 'a/* (-> pat second unform))
      ambiparse/? (list 'a/? (-> pat second unform))
      ambiparse/-filter (list 'a/filter (second pat) (-> pat (nth 2) unform))
      ambiparse/-prefer (list 'a/prefer (second pat) (-> pat (nth 2) unform))
      ambiparse/scope (list 'a/scope (second pat)))
    pat))

(defn edge-label [x]
  (if x
    (str (-> x :prefix ::a/begin :idx) " - " (-> x :prefix ::a/end :idx) "\n"
         "pre: " (-> x :prefix ::a/value pps)
         "env " (-> x :prefix ::a/env pps)
         (when-let [cont (:continue x)]
           (str "cont: " (->> cont (map unform) pps))))
    ""))

(defn identify [ids k]
  (or (@ids k)
      (let [id (inc (count @ids))]
        (swap! ids assoc k id)
        id)))

(defn node-label [{:keys [pat tail? env]}]
  (binding [*print-level* 3]
    (str (-> pat unform pr-str) \newline
         "env: " (pps env))))

(defn pos-node [input]
  (let [label (->> input count range
                   (map #(str "<i" % "> " % " " (nth input %)))
                   (interpose " | ")
                   (apply str))]
  ["pos" {:shape "record" :label label}]))

(defn to-dorothy [{:keys [graph input]}]
  (let [ids (atom {})]
    [(pos-node input)
     (for [[i ks] (map vector (range) graph)
           [k {:keys [edges]}] ks
           :let [{:keys [tail?]} (:ctx k)
                 src-id (identify ids k)]]
       ;; Nodes.
       (list [src-id {:label (node-label k)
                      :penwidth (if tail? 3 1)}]
             ;; Position edge.
             [(str "pos:i" i) src-id {:headlabel (str i) :style "dotted"}]
             ;; Edges.
             (for [[dst decorators] edges
                   :let [dst-id (identify ids dst)]
                   decorator decorators]
               [src-id dst-id {:label (edge-label decorator)}])))]))

(defn show! [state]
  (-> state to-dorothy d/digraph d/dot d/show!))
