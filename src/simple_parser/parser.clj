(ns simple-parser.parser
  (:require [simple-parser.status :as status]
            [simple-parser.location :as location]
            [simple-parser.error :as error]
            [simple-parser.result :as result]
            [clojure.string :refer [starts-with? join]]
            [simple-parser.util :as util]))

;; 因为Clojure是动态语言，有很多类型可以直接使用map、vector等代替。
;; 实际上也可以使用deftype、defrecord、defprotocol来实现，会比较更强类型一些，
;; 而本ParserCombintor是简单为主，因此直接使用基本的函数行为
;; parser 的实际类型是: fn (status) -> result

;; parser需要lazy的，在组合时防止在参数时就发生求值
;; 这个需要写成宏，不能写成函数，因为参数总是在提前求值的，写成函数时，参数的递归会一直继续下去
;; (defn lazy [x]
;;   [:lazy (fn [] x)])

(defn de-lazy [x]
  (loop [p x]
    (if (vector? p)
      (if (= (first p) :lazy)
        (recur ((nth p 1)))
        p)
      p)))

;; 找出字符串s1在offset之后，与字符串s2的第一个不相同字符的下标
;; 当s2与s1在offset之后全符合时（start-with），则返回:all-match-after-offset
(defn- first-not-match [s1 offset s2]
  (let [size1 (count s1)
        size2 (count s2)
        sub1 (util/slice s1 offset)
        subsize1 (- size1 offset)]
    (if (< subsize1 size2)
      0
      (loop [i 0]
        (if (and (< i subsize1)
                 (< i size2))
          (if-not (= (nth sub1 i) (nth s2 i))
            i
            (recur (inc i)))
          (if (>= subsize1 size2)
            :all-match-after-offset
            subsize1))))))

(defn exec [parser status]
  (let [p (de-lazy parser)]
    (p status)))

(defn label [parser msg]
  (fn [status]
    (let [res (exec parser status)]
      (result/map-error #(error/label-error %1 msg) res))))

(defn scope [parser msg]
  (fn [status]
    (let [res (exec parser status)]
      (result/map-error #(error/push-error %1 status msg) res))))

(defn attempt [parser]
  (fn [status]
    (result/uncommit (exec parser status))))

(defn slice [parser]
  (fn [status]
    (let [[success? _data n :as result] (exec parser status)]
      (if (= success? :success)
        [:success (status/get-slice status n) n]
        result))))

(defn flat-map [parser f]
  (fn [status]
    (let [[success? data n :as result] (exec parser status)]
      (if (= success? :success)
        (-> (exec (f data) (status/advance-by status n))
            (result/add-commit (not= n 0))
            (result/advance-success n))
        result))))

(defn succeed [a]
  (fn [_status]
    [:success a 0]))

(defn fmap [parser f]
  [:lazy (fn []
           (flat-map parser #(succeed (f %1))))])

(defn fmap2 [parser1 parser2 f]
  [:lazy (fn []
           (flat-map parser1
                     (fn [a] (fmap parser2
                                   (fn [b] (f a b))))))])

(defn product [parser1 parser2]
  [:lazy (fn []
           (fmap2 parser1 parser2 #(vec [%1 %2])))])

(defn as [parser a]
  [:lazy (fn []
           (fmap (slice parser)
                 (fn [_] a)))])

(defn and' [parser1 parser2]
  [:lazy (fn []
           (flat-map parser1
                     (fn [_] parser2)))])

(defn or' [parser1 parser2]
  (fn [status]
    (let [[success? _ _ :as result] (exec parser1 status)]
      (if (= success? :success)
        result
        (exec parser2 status)))))

(defn *> [parser1 parser2]
  [:lazy (fn []
           (-> (slice parser1)
               (fmap2 parser2 (fn [_ b] b))))])

(defn <* [parser1 parser2]
  [:lazy (fn []
           (fmap2 parser1 (slice parser2) (fn [a _] a)))])

(defn many [parser]
  [:lazy (fn []
           (or' (fmap2 parser (many parser) #(conj %2 %1))
                (succeed '())))])

(defn many1 [parser]
  [:lazy (fn [] (fmap2 parser (many parser) #(conj %2 %1)))])

(defn seq1 [parser1 parser2]
  [:lazy (fn []
           (-> (fmap2 parser1
                      (many (*> parser2 parser1))
                      #(conj %2 %1))))])

(defn seq' [parser1 parser2]
  [:lazy (fn [] (-> (seq1 parser1 parser2)
                    (or' (succeed '()))))])

(defn surround [parser start end]
  [:lazy (fn [] (*> start (<* parser end)))])

(defn string [s]
  (fn [[source offset :as loc]]
    (let [not-match (first-not-match source offset s)
          msg (str "'" s "'")]
      (if (= not-match :all-match-after-offset)
        [:success s (count s)]
        [:failure (-> loc
                      (status/advance-by not-match)
                      (location/loc-to-error msg)) (not= not-match 0)]))))

(defn char' [c]
  [:lazy (fn []
           (fmap (string (str c)) #(nth %1 0)))])

(defn check-first-char [s chars]
  (if (empty? s)
    [:empty nil]
    (let [c (nth s 0)]
      (if (some #{c} chars)
        [:found c]
        [:not-found c]))))

(defn char-in [& chars]
  (fn [[source offset :as status]]
    (let [sub (util/slice source offset)
          [ok? c] (check-first-char sub chars)]
      (if (= ok? :found)
        [:success c 1]
        [:failure (let [col (location/loc-col status)
                        adv (status/advance-by status col)
                        err (location/loc-to-error adv (str "expected " c))]
                    err) false]))))

(defn char-not-in [& chars]
  (fn [[source offset :as status]]
    (let [sub (util/slice source offset)
          [ok? c] (check-first-char sub chars)]
      (if (= ok? :not-found)
        [:success c 1]
        [:failure (let [col (location/loc-col status)
                        adv (status/advance-by status col)
                        err (location/loc-to-error adv (str "not expected " c))]
                    err) false]))))

(defn re-find-once [r input]
  (let [res (re-find r input)]
    (cond
      (nil? res) false
      (vector? res) (first res)
      :else res)))

(defn regex [r]
  (fn [status]
    (let [msg (str "regex: " r)
          input (status/get-input status)
          res (re-find-once r input)]
      (if (and res (starts-with? input res))
        [:success res (count res)]
        [:failure (location/loc-to-error status msg) false]))))

(def whitespace
  [:lazy (fn [] (regex #"\s*"))])

(def digits
  [:lazy (fn [] (regex #"\d+"))])

(defn thru [s]
  [:lazy (fn []
           (regex (re-pattern (str ".*?" (java.util.regex.Pattern/quote s)))))])

(defn- remove-str-last [s]
  (if (empty? s)
    s
    (util/slice s 0 (dec (count s)))))

(def quoted
  [:lazy (fn []
           (-> (string "\"")
               (*> (thru "\""))
               (fmap #(remove-str-last %1))))])

(defn >> [x] (fn [_] x))

(def escaped
  [:lazy (fn []
           (let [a (fmap (string "\\\"") (>> "\""))
                 b (fmap (string "\\\\") (>> "\\"))
                 c (fmap (string "\\/") (>> "/"))
                 d (fmap (string "\\b") (>> "\b"))
                 e (fmap (string "\\f") (>> "\f"))
                 f (fmap (string "\\n") (>> "\n"))
                 g (fmap (string "\\r") (>> "\r"))
                 h (fmap (string "\\t") (>> "\t"))]
             (-> a
                 (or' b)
                 (or' c)
                 (or' d)
                 (or' e)
                 (or' f)
                 (or' g)
                 (or' h))))])

(def escaped-quoted
  [:lazy (fn []
           (-> (or' escaped (char-not-in \" \\))
               (many1)
               (surround (string "\"") (string "\""))
               (fmap #(join %1))))])

(defn token [parser]
  [:lazy (fn []
           (surround parser whitespace whitespace))])

(def double-string
  [:lazy (fn []
           (token (regex #"[-+]?([0-9]*\.)?[0-9]+([eE][-+]?[0-9]+)?")))])

(def eof
  [:lazy (fn []
           (label (regex #"\z") "unexpected trailing characters"))])

(defn file-root [parser]
  [:lazy (fn []
           (<* parser eof))])
