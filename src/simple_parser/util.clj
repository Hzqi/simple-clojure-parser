(ns simple-parser.util)

(defn slice
  ([s start end]
   (if (> end (count s))
     (subs s start (count s))
     (subs s start end)))
  ([s start] (subs s start)))
