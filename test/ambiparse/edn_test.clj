(ns ambiparse.edn-test
  (:require [clojure.test :refer :all]
            [ambiparse :as a]))

;; EDN is LL(1), so this is a poor test of capability, but a decent
;; performance baseline. Note: This is for test purposes only and will never
;; be remotely feature complete.

(declare Form)

(def LineComment
  (a/cat \; (a/* (a/pred #(not= \newline %))) \newline))

(def Space
  (a/+ (a/alt \space \tab \newline \, LineComment)))

(def Forms
  (a/cat (a/? Space)
         (a/label :forms (a/interpose Space #'Form))
         (a/? Space)))

(def Digit
  (a/rule a/digit
          (- (-> % ::a/value int) (int \0))))

(def Int
  (a/rule (a/+ Digit)
          (reduce (fn [n d]
                    (+ (* n 10) d))
                  0 (::a/value %))))

(def Num
  Int)

(def SymbolChars
  (a/pred #((set ".*+!-_?$%&=<>") %)))

(def BaseName
  (a/rule (a/cons (a/alt a/alpha SymbolChars)
                  (a/* (a/alt a/alpha Digit SymbolChars)))
          (->> % ::a/value (apply str))))

(def Namespace
  BaseName)

(def Name
  (a/alt BaseName "/"))

(def Symbol
  (a/alt (a/rule (a/cat (a/label :ns Namespace) \/ (a/label :name Name))
                 (symbol (:ns %) (:name %)))
         (a/rule Name (-> % ::a/value symbol))))

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
  (a/rule (a/cat \\ (a/label :char (a/alt NamedChar
                                          ;;TODO Use a negative match.
                                          (a/pred #(not= \space %)))))
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
    "a/b"
    "/"
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
