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

(defn node-label [{:keys [i pat tail? env]}]
  (binding [*print-level* 3]
    (str i " " (-> pat unform pr-str) \newline
         "env: " (pps env))))

(defn to-dorothy [g]
  (let [ids (atom {})]
     (for [[i ks] (map vector (range) g)
           [{:keys [tail?] :as k} {:keys [edges]}] ks
           :let [src-id (identify ids k)]]
       ;; Nodes.
       (list [src-id {:label (node-label k)
                      :penwidth (if tail? 3 1)}]
             ;; Edges.
             (for [[dst decorators] edges
                   :let [dst-id (identify ids dst)]
                   decorator decorators]
               [src-id dst-id {:label (edge-label decorator)}])))))

(defn show! [g]
  (-> g to-dorothy d/digraph d/dot d/show!))
