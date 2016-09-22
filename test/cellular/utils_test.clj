(ns cellular.utils_test
  (:use [testa.core :only (suite)])
  (:require [cellular.utils :as util]))

(suite
  "utils"
  (:fact if-nonnil-let:0
         (util/if-nonnil-let* []
           :then
           :else)
         :is
         :then)
  (:fact if-nonnil-let:1
         (util/if-nonnil-let* [a 1]
           a)
         :is
         1)
  (:fact if-nonnil-let:2
         (util/if-nonnil-let* [a false]
           a)
         :is
         false)
  (:fact if-nonnil-let:3
         (util/if-nonnil-let* [a 1
                               b (inc a)]
           b
           :else)
         :is
         2)
  (:fact if-nonnil-let:4
         (util/if-nonnil-let* [a 1
                               b nil]
           :then
           :else)
         :is
         :else)
  (:fact when-nonnil-let
         (let [sb (StringBuilder.)]
           (util/when-nonnil-let* [a \a b \b]
             (.append sb a)
             (.append sb b))
           (str sb))
         :is
         "ab"))
           
