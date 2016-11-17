(ns ambiparse.util)

(defmacro update! [v f & args]
  `(set! ~v (~f ~v ~@args)))
