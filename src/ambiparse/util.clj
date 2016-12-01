(ns ambiparse.util)

(defmacro change! [v f & args]
  `(set! ~v (~f ~v ~@args)))

(defn compare-key [f x y]
  (compare (f x) (f y)))

(defn comparator-key [f]
  (comparator #(compare-key f %1 %2)))
