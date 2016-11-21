(ns ambiparse.edn-test
  (:require [clojure.test :refer :all]
            [ambiparse :as a]))

;; EDN is LL(1), so this is a poor test of capability, but a decent
;; performance baseline. Note: This is for test purposes only and will never
;; be remotely feature complete.

;;TODO: Make use of character classes.

(declare Form)

(def Space
  (a/+ (a/alt \space \tab \newline \,)))

(def Forms
  (a/cat (a/? Space)
         (a/label :forms (a/interpose Space #'Form))
         (a/? Space)))

(def Digit
  (a/rule (apply a/alt (map #(char (+ (int \0) %)) (range 10)))
          (- (-> % ::a/value int) (int \0))))

(def Int
  (a/rule (a/+ Digit)
          (reduce (fn [n d]
                    (+ (* n 10) d))
                  0 (::a/value %))))

(def Num
  Int)

(def Alpha
  ;;TODO: Case insensitive.
  (apply a/alt (map #(char (+ (int \a) %)) (range 26))))

(def SymbolChars
  (apply a/alt ".*+!-_?$%&=<>"))

;;TODO: Put cons in to lib?
(def Symbol
  (a/rule (a/cons (a/alt Alpha SymbolChars)
                  (a/* (a/alt Alpha Digit SymbolChars)))
          (symbol (apply str (::a/value %)))))

(def Keyword
  (a/rule (a/cat \: (a/label :symbol Symbol))
          (-> % :symbol keyword)))

(def Str
  \x) ;XXX

(def NamedChar
  (a/alt (a/rule "newline" \newline)
         (a/rule "return" \return)
         (a/rule "space" \space)
         (a/rule "tab" \tab)))

(def Char
  ;XXX Add any, disallow whitespace after the slash.
  (a/rule (a/cat \\ (a/label :char (a/alt NamedChar #_ANY)))
          (:char %)))

(def List
  (a/rule (a/cat \( Forms \))
          (-> % :forms list* (or ()))))

(def Map
  (a/rule (a/cat \{ Forms \})
          (->> % :forms (partition 2) (map vec) (into {}))))

(def Vector
  (a/rule (a/cat \[ Forms \])
          (-> % :forms vec)))

(def Form
  (a/alt Num Symbol Keyword Str Char List Map Vector))

(deftest edn-test
  (are [s] (= (a/parse! Form s) (read-string s))
    "15"
    "xyz"
    ":abc"
    ;XXX "\\x"
    "\\newline"
    "[]"
    "[1]"
    "[1 2 3]"
    "[ 1 2 3  ]"
    "{}"
    "{:x 1}"
    "{:x 1, :y 2}"
    "()"
    "(a b c)"
    ))

(comment
  (a/parse! Form "{:x 1 :y 2}")
)
