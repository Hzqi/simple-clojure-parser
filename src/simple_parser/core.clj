(ns simple-parser.core
  (:require [simple-parser.parser :as p]
            [simple-parser.result :as r]))

(defn run-parser [parser input]
  (let [res (p/exec parser [input 0])]
    (r/print-error res)
    res))
