(ns simple-parser.status
  (:require [simple-parser.util :as util]))

;; ParserStatus 解析器的状态信息
;; 使用location作为解析起的中间状态 [ source offset ]

(defn advance-by [[source offset] num]
  [source (+ offset num)])

(defn get-input [[source offset]]
  (util/slice source offset))

(defn get-slice [[source offset] n]
  (util/slice source offset (+ offset n)))
