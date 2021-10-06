(ns simple-parser.v1.location
  (:require [clojure.string :refer [last-index-of, split-lines, join]]
            [simple-parser.v1.util :as util]))

;; Location 表示解析时的位置信息
;; [ source, offset ] 位置信息的tuple，source是输入源， offset是位置下标

;; 计算字符串又多少行
(defn count-line [s]
  (-> (re-seq #"\n" s)
      (count)
      (inc)))

;; 计算当前位置在第几行
(defn loc-line [[source, offset]]
  (-> source
      (util/slice 0 (inc offset))
      (count-line)))

;; 计算当前位置是当前行的第几个位置
(defn loc-col [[source, offset]]
  (-> source
      (util/slice 0 (inc offset))
      (last-index-of "\n")
      (#(if (nil? %1)
          (inc offset)
          (- offset %1)))))

;; 构建位置错误栈
(defn loc-to-error [loc msg]
  [[loc msg]])

;; 前进n位置
(defn loc-advance [[source, offset] n]
  [source, (+ offset n)])

;; 计算返回offset当当前行字符串
(defn loc-current-line [[source, _offset :as loc]]
  (let [lines (split-lines source)
        line-idx (loc-line loc)]
    (if (> (count lines) 1)
      (first (drop (dec line-idx) lines))
      (first lines))))

;; 计算返回当前行的offset的位置
(defn loc-column-caret [loc]
  (let [col (loc-col loc)
        blank (join (repeat (dec col) "-"))]
    (join "" [blank "^"])))
