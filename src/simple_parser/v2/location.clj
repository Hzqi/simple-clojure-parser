(ns simple-parser.v2.location
  (:require [clojure.string :refer [last-index-of split-lines join]]
            [simple-parser.v2.util :as util]))

;; 位置信息的record
(defrecord Location [source offset])

(defn loc-line
  "计算当前位置在第几行"
  [loc]
  (-> (:source loc)
      (util/slice 0 (inc (:offset loc)))
      (util/count-line)))

(defn loc-col
  "计算当前位置在第几列"
  [loc]
  (let [source (:source loc)
        offset (:offset loc)]
    (-> source
        (util/slice 0 (inc offset))
        (last-index-of "\n")
        (#(if (nil? %1)
            (inc offset)
            (- offset %1))))))

(defn loc-to-error
  "构建位置错误栈"
  [loc msg]
  [[loc msg]])

(defn loc-advance
  "前进n位置"
  [loc n]
  (let [source (:source loc)
        offset (:offset loc)]
    (Location. source (+ offset n))))

(defn loc-current-line
  "计算返回offset当前行字符串"
  [loc]
  (let [lines (split-lines (:source loc))
        line-idx (loc-line loc)]
    (if (> (count lines) 1)
      (first (drop (dec line-idx) lines))
      (first lines))))

(defn loc-column-caret
  "计算并返回当前行的offset的位置"
  [loc]
  (let [col (loc-col loc)
        blank (join (repeat (dec col) "-"))]
    (join "" [blank "^"])))
