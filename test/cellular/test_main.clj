(ns cellular.test-main
  (:use [testa.core :only (load-cases main)])
  (:require cellular.stream
            cellular.basics
            cellular.utils_test
            cellular.combinators
            cellular.helpers)
  (:gen-class))

(defn -main [& args]
  (->>
    (load-cases
      'cellular.stream
      'cellular.basics
      'cellular.utils_test
      'cellular.combinators
      'cellular.helpers)
    (main args)))
