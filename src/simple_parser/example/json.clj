(ns simple-parser.example.json
  (:require [simple-parser.parser :refer :all]))

(declare root)
(declare obj)
(declare ary)
(declare keyval)
(declare jval)
(declare lit)

;; root ::= [ whitespace ] (object | array)
(def root
  [:lazy (fn [] (*> whitespace (or' obj ary)))])

;; object ::= '{' [ whitespace ] key-val [ whitespace ] [, token(key-val) ... ] '}'
(def obj
  [:lazy (fn []
           (-> (token keyval)
               (seq' (string ","))
               (surround (string "{") (string "}"))
               (fmap #(into {} %1))))])

;; array ::= '[' [whitespace] value [whitespace] [, value ...] ']'
(def ary
  [:lazy (fn []
           (-> (token jval)
               (seq' (string ","))
               (surround (string "[") (string "]"))))])

;; key-val ::= quoted [whitespace] ':' [whitespace] value
(def keyval
  [:lazy (fn []
           (product quoted (*> (token (string ":")) jval)))])

;; value ::= lit | object | array
(def jval
  [:lazy (fn []
           (-> lit (or' obj) (or' ary)))])

;; lit ::= null | number | escaped-quoted | true | false
(def lit
  [:lazy (fn []
           (let [nul (string "null")
                 d double-string
                 s escaped-quoted
                 t (string "true")
                 f (string "false")]
             (-> nul (or' d) (or' s) (or' t) (or' f))))])
