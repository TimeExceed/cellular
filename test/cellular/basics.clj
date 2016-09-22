(ns cellular.basics
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
  "expect-char"
  (:fact expect-char:match
         (apply-parser
           "ab"
           (cell/expect-char \a))
         :is
         [{:result \a} [\b :eof]])
  (:fact expect-char:unmatch
         (apply-parser
           "b"
           (cell/expect-char \a))
         :is
         [{:error "unexpect \\b"} [\b :eof]])
  (:fact expect-char:eof
         (apply-parser
           ""
           (cell/expect-char \a))
         :is
         [{:error "unexpect :eof"} [:eof]])
  (:fact expect-char-if:positive
         (apply-parser
           "a"
           (cell/expect-char-if #{\a}))
         :is
         [{:result \a} [:eof]])
  (:fact expect-char-if:negative
         (apply-parser
           "b"
           (cell/expect-char-if #{\a}))
         :is
         [{:error "unexpect \\b"} [\b :eof]]))

(suite
  "expect string"
  (:fact expect-str:positive
         (apply-parser
           "ab"
           (cell/expect-str "ab"))
         :is
         [{:result "ab"} [:eof]])
  (:fact expect-str:negative
         (apply-parser
           "ab"
           (cell/expect-str "ba"))
         :is
         [{:error "expect \"ba\""} [\a \b :eof]]))

(suite
  "skip"
  (:fact skip-while
         (apply-parser
           "abc"
           (cell/skip-while #{\a \b}))
         :is
         [{:result [\a \b]} [\c :eof]]))
