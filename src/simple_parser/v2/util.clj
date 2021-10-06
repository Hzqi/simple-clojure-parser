(ns simple-parser.v2.util)

(defn slice
  "安全式的子序列"
  ([s start end]
   (if (> end (count s))
     (subs s start (count s))
     (subs s start end)))
  ([s start] (subs s start)))

(defn count-line
  "计算字符串的行数"
  [s]
  (-> (re-seq #"\n" s)
      (count)
      (inc)))
