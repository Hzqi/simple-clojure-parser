(ns simple-parser.v1.core
  (:require [simple-parser.v1.parser :as p]
            [simple-parser.v1.result :as r]))

(defn run-parser [parser input]
  (let [res (p/exec parser [input 0])]
    (r/print-error res)
    res))
