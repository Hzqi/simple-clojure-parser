(ns simple-parser.result
  (:require [simple-parser.error :as error]))

;; ParserResult 表示解析后的结果值
;; [:success result length] 解析成功时的tuple，result为结果，length为成功的长度
;; [:failure parse-error commited?] 解析失败时的tuple，parse-error为错误信息栈，commited？为是否已提交

(defn uncommit [[success? data-or-err _len-or-commited :as result]]
  (if (= success? :success)
    result
    [:failure data-or-err false]))

(defn add-commit [[success? data-or-err len-or-commited :as result]
                  commit]
  (if (= success? :success)
    result
    [:failure data-or-err (or commit len-or-commited)]))

(defn map-error [f [success? data-or-err len-or-commited :as result]]
  (if (= success? :success)
    result
    [:failure (f data-or-err) len-or-commited]))

(defn advance-success [[success? data-or-err len-or-commited :as result]
                       n]
  (if (= success? :failure)
    result
    [:success data-or-err (+ len-or-commited n)]))

(defn print-error [[success? data-or-err _len-or-commited]]
  (when (= success? :failure)
    (error/println-error-stack data-or-err)))
