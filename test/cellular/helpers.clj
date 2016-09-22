(ns cellular.helpers
  (:use [testa.core :only (suite)])
  (:require [cellular.core :as cell]))

(suite
  "helpers"
  (:fact extract-str-between
         (let [strm (cell/str->stream "ab")]
           (cell/collect-str-between strm (next strm)))
         :is
         "a"))
