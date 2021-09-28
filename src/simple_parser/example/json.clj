(ns simple-parser.example.json
  (:require [simple-parser.parser :refer [*>
                                          double-string
                                          escaped-quoted
                                          fmap
                                          or'
                                          product
                                          quoted
                                          seq'
                                          string
                                          surround
                                          token
                                          whitespace]]))

(declare root)
(declare obj)
(declare ary)
(declare keyval)
(declare jval)
(declare lit)

;; root ::= [ whitespace ] (object | array)
(def root
  (delay (*> whitespace (or' obj ary))))

;; object ::= '{' [ whitespace ] key-val [ whitespace ] [, token(key-val) ... ] '}'
(def obj
  (delay (-> (token keyval)
             (seq' (string ","))
             (surround (string "{") (string "}"))
             (fmap #(into {} %1)))))

;; array ::= '[' [whitespace] value [whitespace] [, value ...] ']'
(def ary
  (delay (-> (token jval)
             (seq' (string ","))
             (surround (string "[") (string "]")))))

;; key-val ::= quoted [whitespace] ':' [whitespace] value
(def keyval
  (delay (product quoted (*> (token (string ":")) jval))))

;; value ::= lit | object | array
(def jval
  (delay (-> lit (or' obj) (or' ary))))

;; lit ::= null | number | escaped-quoted | true | false
(def lit
  (delay
   (let [nul (string "null")
         d double-string
         s escaped-quoted
         t (string "true")
         f (string "false")]
     (-> nul (or' d) (or' s) (or' t) (or' f)))))
