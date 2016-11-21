(ns ambiparse.benchmark
  (:require [clojure.java.io :as io]
            [ambiparse :as a]
            [ambiparse.edn-test :refer [Forms]])
  (:import (java.io StringReader PushbackReader)))

(def edn (slurp (io/resource "ambiparse/edn_test.clj")))

(defn read-all [s]
  (let [r (-> s StringReader. PushbackReader.)]
    (->> (repeatedly #(read {:eof ::eof} r))
         (take-while #(not= % ::eof))
         vec)))

(prn 'reader)
(time
  (dotimes [_ 1000]
    (read-all edn)))

(prn 'ambiparse)
(time
  (dotimes [_ 1]
    (a/parse! Forms edn)))

