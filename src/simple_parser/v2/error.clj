(ns simple-parser.v2.error
  (:require [simple-parser.v2.location :as location]))

;; 位置错误信息节点
(defrecord ErrorItem [loc msg])

(defn loc-to-error
  "构建位置错误栈"
  [loc msg]
  [(ErrorItem. loc msg)])

(defn push-error
  "追加错误"
  [stack loc msg]
  (conj stack (ErrorItem. loc msg)))

(defn last-error-loc [stack]
  (when (seq stack)
    (:loc (last stack))))

(defn label-error
  "给错误打上标签"
  [stack msg]
  (let [last-loc (last-error-loc stack)]
    (if-not (nil? last-loc)
      [(ErrorItem. last-loc msg)]
      stack)))

(defn println-error-stack [stack]
  (if (seq stack)
    (doseq [{loc :loc, msg :msg} stack]
      (println msg ":")
      (println (location/loc-current-line loc))
      (println (location/loc-column-caret loc)))
    (println "no error message")))
