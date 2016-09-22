(ns cellular.combinators
  (:use [testa.core :only (suite)])
  (:require [cellular.core :as cell]))

(defn- extract-result [x]
  (let [[result strm] x]
    [result (vec (map :result strm))]))

(defn- apply-parser [s parser]
  (-> s
      (cell/str->stream)
      (parser)
      (extract-result)))

(suite
  "optional"
  (:fact optional:positive
         (apply-parser "a" (cell/optional (cell/expect-char \a)))
         :is
         [{:result \a} [:eof]])
  (:fact optional:negative
         (apply-parser "a" (cell/optional (cell/expect-char \b)))
         :is
         [{:result :none} [\a :eof]]))

(suite
  "choice"
  (:fact choice:first
         (apply-parser
           "a"
           (cell/choice (cell/expect-char \a)
                        (cell/expect-char \b)))
         :is
         [{:result \a} [:eof]])
  (:fact choice:second
         (apply-parser
           "b"
           (cell/choice (cell/expect-char \a)
                        (cell/expect-char \b)))
         :is
         [{:result \b} [:eof]])
  (:fact choice:unmatch
         (apply-parser
           "c"
           (cell/choice (cell/expect-char \a)
                        (cell/expect-char \b)))
         :is
         [{:error "match nothing"} [\c :eof]])
  (:fact choice*:first
         (apply-parser
           "a"
           (cell/choice*
             (cell/expect-char \a) (fn [x] (if (= x \a) :a))
             (cell/expect-char \b) (fn [x] (if (= x \b) :b))))
         :is
         [{:result :a} [:eof]])
  (:fact choice*:second
         (apply-parser
           "b"
           (cell/choice*
             (cell/expect-char \a) (fn [x] (if (= x \a) :a))
             (cell/expect-char \b) (fn [x] (if (= x \b) :b))))
         :is
         [{:result :b} [:eof]])
  (:fact choice*:default
         (apply-parser
           "c"
           (cell/choice*
             (cell/expect-char \a) (fn [x] (if (= x \a) :a))
             (cell/expect-char \b) (fn [x] (if (= x \b) :b))
             :default))
         :is
         [{:result :default} [\c :eof]])
  (:fact choice*:unmatch
         (apply-parser
           "c"
           (cell/choice* (cell/expect-char \a) (fn [_] :a)
                         (cell/expect-char \b) (fn [_] :b)))
         :is
         [{:error "match nothing"} [\c :eof]]))
  
(suite
  "many"
  (:fact many:0
         (apply-parser
           ""
           (cell/many (cell/expect-char-if #{\a \b})))
         :is
         [{:result []} [:eof]])
  (:fact many:1
         (apply-parser
           "a"
           (cell/many (cell/expect-char-if #{\a \b})))
         :is
         [{:result [\a]} [:eof]])
  (:fact many:2
         (apply-parser
           "ab"
           (cell/many (cell/expect-char-if #{\a \b})))
         :is
         [{:result [\a \b]} [:eof]])
  (:fact many1:0
         (apply-parser
           ""
           (cell/many1 (cell/expect-char-if #{\a \b})))
         :is
         [{:error "match nothing in many1"} [:eof]])
  (:fact many1:1
         (apply-parser
           "a"
           (cell/many1 (cell/expect-char-if #{\a \b})))
         :is
         [{:result [\a]} [:eof]])
  (:fact many1:2
         (apply-parser
           "ab"
           (cell/many1 (cell/expect-char-if #{\a \b})))
         :is
         [{:result [\a \b]} [:eof]]))

(suite
  "chain"
  (:fact chain:match
         (apply-parser
           "ab"
           (cell/chain (cell/expect-char \a) (cell/expect-char \b)))
         :is
         [{:result [\a \b]} [:eof]])
  (:fact chain:mismatch:1
         (apply-parser
           "cb"
           (cell/chain (cell/expect-char \a) (cell/expect-char \b)))
         :is
         [{:error "unexpect \\c"} [\c \b :eof]])
  (:fact chain:mismatch:2
         (apply-parser
           "ac"
           (cell/chain (cell/expect-char \a) (cell/expect-char \b)))
         :is
         [{:error "unexpect \\c"} [\c :eof]]))

(suite
  "foresee"
  (:fact foresee
         (apply-parser
           "ab"
           (cell/foresee (cell/expect-char \a)))
         :is
         [{:result \a} [\a \b :eof]]))

(suite
  "between"
  (:fact between
         (apply-parser
           "[a]"
           (cell/between (cell/expect-char \[)
                         (cell/expect-char \a)
                         (cell/expect-char \])))
         :is
         [{:result \a} [:eof]]))

(suite
  "separated list"
  (:fact separated-list
         (apply-parser
           "a,b"
           (cell/separated-list (cell/expect-any-char) (cell/expect-char \,)))
         :is
         [{:result [\a \b]} [:eof]]))
