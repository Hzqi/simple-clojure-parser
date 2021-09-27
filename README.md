# simple_parser

这是一个非常简单的解析器组合器，没有其他东西的引入，仅使用Clojure自身的结构。

因为暂时只是一个玩具，文档可能会后续再完善。

## Json例子
```bnf
root ::= [ whitespace ] (object | array)
object ::= '{' [ whitespace ] key-val [ whitespace ] [, token(key-val) ... ] '}'
array ::= '[' [whitespace] value [whitespace] [, value ...] ']'
key-val ::= quoted-string [whitespace] ':' [whitespace] value
value ::= lit | object | array
lit ::= null | number | escaped-quoted-string | true | false
```

```clojure
(ns simple-parser.example.json
  (:require [simple-parser.parser :refer :all]))

(declare root)
(declare obj)
(declare ary)
(declare keyval)
(declare jval)
(declare lit)

(def root
  [:lazy (fn [] (*> whitespace (or' obj ary)))])

(def obj
  [:lazy (fn []
           (-> (token keyval)
               (seq' (string ","))
               (surround (string "{") (string "}"))
               (fmap #(into {} %1))))])

(def ary
  [:lazy (fn []
           (-> (token jval)
               (seq' (string ","))
               (surround (string "[") (string "]"))))])

(def keyval
  [:lazy (fn []
           (product quoted (*> (token (string ":")) jval)))])

(def jval
  [:lazy (fn []
           (-> lit (or' obj) (or' ary)))])

(def lit
  [:lazy (fn []
           (let [nul (string "null")
                 d double-string
                 s escaped-quoted
                 t (string "true")
                 f (string "false")]
             (-> nul (or' d) (or' s) (or' t) (or' f))))])
```

使用时是直接调用（暂未包含工程向的工具函数，如包装结果，打印错误等）：

```clojure
(simple-parser.parser/exec root "{
    \"name\" : \"jacky\",
    \"age\" : 20 ,
    \"skill\": [\"go\", \"elixir\", \"java\", \"clojure\"]
}")
;; => [:success {
;;        "name" "jacky"
;;        "age" 20
;;        "skill"  '("go" "elixir" "java" "clojure")} 107]
```
