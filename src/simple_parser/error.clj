(ns simple-parser.error
  (:require [simple-parser.location :as location]))

;; ParserError 表示解析错误时的信息
;; '( [location, msg], ... ) 解析失败时的信息栈，使用list来实现的栈

;; 追加错误
(defn push-error [stack loc msg]
  (conj stack [loc, msg]))

(defn last-error-loc [stack]
  (when (seq stack)
    (nth (last stack) 0)))

(defn label-error [stack msg]
  (let [last-loc (last-error-loc stack)]
    (if-not (nil? last-loc)
      [[last-loc msg]]
      stack)))

(defn println-error-stack [stack]
  (if (seq stack)
    (doseq [[loc msg] stack]
      (println msg ":")
      (println (location/loc-current-line loc))
      (println (location/loc-column-caret loc)))
    (println "no error message")))
