;; The MIT License (MIT)

;; Copyright (c) 2016 tyf00@aliyun.com, picked from https://github.com/TimeExceed/cellular

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(ns cellular.core
  (:use
   [clojure.set]
   [cellular.utils :only (if-nonnil-let* when-nonnil-let* enumerate)])
  (:import
   [java.io Reader StringReader]))

;; streaming

(defn reader->stream [^Reader rdr]
  (try 
    (let [ch (.read rdr)]
      (if (neg? ch)
        [{:result :eof}]
        (lazy-seq (cons {:result (char ch)}
                        (reader->stream rdr)))))
    (catch Exception ex
      {:error ex})))

(defn str->stream [^String str]
  (let [rdr (StringReader. str)]
    (reader->stream rdr)))

(defn ->stream [xs]
  (for [x xs] {:result x}))

(defn offset-stream [strm]
  (for [[offset item] (enumerate strm)] (assoc item :offset offset)))

(defn- position-stream' [strm row column]
  (when (seq strm)
    (let [[head & tail] strm]
      (if (= (:result head) \newline)
        (lazy-seq (cons (assoc head :row row :column column)
                        (position-stream' tail (inc row) 1)))
        (lazy-seq (cons (assoc head :row row :column column)
                        (position-stream' tail row (inc column))))))))

(defn position-stream [strm]
  (position-stream' strm 1 1))

;; basics

(defn- issue [item msg]
  (-> item
      (dissoc :result)
      (assoc :error msg)))

(defn expect-char-if [pred]
  (fn [strm]
    (let [[head & tail] strm]
      (if-not (nil? (:error head))
        [head tail]
        (let [{ch :result} head]
          (if (pred ch)
            [{:result ch} tail]
            [(issue head (format "unexpect %s"(pr-str ch)))
             strm]))))))

(defn expect-char-if-not [pred]
  (expect-char-if #(not (pred %))))

(defn expect-char [ch]
  (expect-char-if #(= % ch)))

(defn expect-any-char []
  (expect-char-if #(not= % :eof)))

(defn- expect-eof-parser [strm]
  (let [[head & tail] strm]
    (if-not (nil? (:error head))
      [head tail]
      (if (= (:result head) :eof)
        [{:result :eof} strm]
        [(issue head "expect :eof") strm]))))

(defn expect-eof []
  expect-eof-parser)

(defn- expect-str-parser' [s strm]
  (if (empty? s)
    strm
    (let [[c & ss] s]
      (if (seq strm)
        (let [[x & xs] strm
              {ch :result} x]
          (if (= c ch)
            (recur ss xs)))))))

(defn- expect-str-parser [s strm]
  (if-nonnil-let* [nxt-strm (expect-str-parser' s strm)]
    [{:result s} nxt-strm]
    [(issue (first strm) (format "expect %s" (pr-str s))) strm]))

(defn expect-str [s]
  (partial expect-str-parser s))


(defn- skip-while-parser' [skipped pred strm]
  (let [[head & tail] strm]
    (if (pred (:result head))
      (do (conj! skipped (:result head))
          (recur skipped pred tail))
      strm)))

(defn- skip-while-parser [pred strm]
  (let [skipped (transient [])
        nxt-strm (skip-while-parser' skipped pred strm)]
    [{:result (persistent! skipped)} nxt-strm]))

(defn skip-while [pred]
  (partial skip-while-parser pred))

;; combinators

(defn- optional-parser [f strm]
  (let [[res nxt-strm :as whole] (f strm)]
    (if (nil? (:result res))
      [{:result :none} strm]
      whole)))

(defn optional [f]
  (partial optional-parser f))

(defn- choice-parser [parsers strm]
  (if-not (seq parsers)
    [(issue (first strm) "match nothing")
     strm]
    (let [[parser & rest-parsers] parsers
          [res nxt-strm :as whole] (parser strm)]
      (if (:result res)
        whole
        (recur rest-parsers strm)))))

(defn choice [& parsers]
  (partial choice-parser parsers))

(defn- choice*-parser [args strm]
  (condp = (count args)
    0 [(issue (first strm) "match nothing") strm]
    1 [{:result (first args)} strm]
    (let [[parser f & rest-args] args
          [res nxt-strm] (parser strm)]
      (if-nonnil-let* [res (:result res)]
        [{:result (f res)} nxt-strm]
        (recur rest-args strm)))))

(defn choice* [& args]
  (partial choice*-parser args))

(defn- many-parser' [xs parser strm]
  (let [[res nxt-strm] (parser strm)]
    (if (nil? (:error res))
      (do
        (conj! xs (:result res))
        (recur xs parser nxt-strm))
      strm)))

(defn- many-parser [parser strm]
  (let [xs (transient [])
        nxt-strm (many-parser' xs parser strm)]
    [{:result (persistent! xs)} nxt-strm]))

(defn many [parser]
  (partial many-parser parser))

(defn- many1-parser [parser strm]
  (let [[res nxt-strm :as whole] (many-parser parser strm)]
    (if-not (nil? (:error res))
      whole
      (if (empty? (:result res))
        [(issue (first strm) "match nothing in many1") strm]
        whole))))

(defn many1 [parser]
  (partial many1-parser parser))

(defn- chain-parser' [res-collector parsers strm]
  (if-not (seq parsers)
    [{} strm]
    (let [[p & ps] parsers
          [res nxt-strm :as whole] (p strm)]
      (if-not (nil? (:error res))
        whole
        (do
          (conj! res-collector (:result res))
          (recur res-collector ps nxt-strm))))))

(defn- chain-parser [parsers strm]
  (let [res (transient [])]
    (let [[err nxt-strm :as whole] (chain-parser' res parsers strm)]
      (if-not (nil? (:error err))
        whole
        [{:result (persistent! res)} nxt-strm]))))

(defn chain [& parsers]
  (partial chain-parser parsers))


(defn- foresee-parser [parser strm]
  (let [[result] (parser strm)]
    [result strm]))
      

(defn foresee [parser]
  (partial foresee-parser parser))

(defn- between-parser [left middle right strm]
  (let [[left-res strm0 :as left-whole] (left strm)]
    (if (nil? (:result left-res))
      left-whole
      (let [[mid-res strm1] (middle strm0)]
        (if (nil? (:result mid-res))
          [mid-res strm]
          (let [[right-res strm2] (right strm1)]
            (if (nil? (:result right-res))
              [right-res strm]
              [mid-res strm2])))))))

(defn between [left middle right]
  (partial between-parser left middle right))

(defn- separated-list-parser'' [result item sep strm]
  (let [[res0 strm0] (sep strm)]
    (if (nil? (:error res0))
      (let [[res1 strm1 :as whole] (item strm0)]
        (if-not (nil? (:error res1))
          whole
          (do (conj! result (:result res1))
              (separated-list-parser'' result item sep strm1))))
      [{:result (persistent! result)} strm])))

(defn- separated-list-parser' [result item sep strm]
  (let [[res0 strm0] (item strm)]
    (if-not (nil? (:error res0))
      [res0 strm]
      (do (conj! result (:result res0))
          (separated-list-parser'' result item sep strm0)))))

(defn- separated-list-parser [item sep strm]
  (let [res-collector (transient [])]
    (separated-list-parser' res-collector item sep strm)))

(defn separated-list [item sep]
  (partial separated-list-parser item sep))

;; helpers

(def digit #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(def hexdigit (union #{\a \b \c \d \e \f \A \B \C \D \E \F} digit))

(def letter 
  (set (for [x (range 128)
             :let [ch (char x)]
             :let [gta (>= (Character/compare ch \a) 0)] 
             :let [ltz (<= (Character/compare ch \z) 0)]
             :let [gtA (>= (Character/compare ch \A) 0)]
             :let [ltZ (<= (Character/compare ch \Z) 0)]
             :when (or (and gta ltz) (and gtA ltZ))]
         ch)))

(def whitespace #{\space \tab \formfeed \newline})

(defn- collect-str-between' [sb start end]
  (when-not (= start end)
    (let [[head & tail] start]
      (.append sb (:result head))
      (collect-str-between' sb tail end))))

(defn collect-str-between [start end]
  (let [sb (StringBuilder.)]
    (collect-str-between' sb start end)
    (str sb)))
