(ns ambiparse.benchmark
  (:require [clojure.java.io :as io]
            [ambiparse :as a]
            [ambiparse.edn-test :refer [Forms]])
  (:import (java.io StringReader PushbackReader)))

(def edn (slurp (io/resource "stuff.edn")))

(defn read-all [s]
  (let [r (-> s StringReader. PushbackReader.)]
    (->> (repeatedly #(read {:eof ::eof} r))
         (take-while #(not= % ::eof))
         vec)))

(comment

  (prn 'reader)
  (time
    (dotimes [_ 100]
      (read-all edn)))

  (prn 'ambiparse)
  (time
    (dotimes [_ 10]
      (a/parse! Forms edn)))

  (subs edn 50)

)
