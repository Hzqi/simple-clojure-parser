(ns simple-parser.v1.util)

(defn slice
  ([s start end]
   (if (> end (count s))
     (subs s start (count s))
     (subs s start end)))
  ([s start] (subs s start)))
