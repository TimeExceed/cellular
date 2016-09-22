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

(ns cellular.utils)

(defmacro if-nonnil-let*
  ([bindings then]
   `(if-nonnil-let* ~bindings ~then nil))
  ([bindings then else]
   (if (seq bindings)
     `(let [~(first bindings) ~(second bindings)]
        (if-not (nil? ~(first bindings))
          (if-nonnil-let* ~(drop 2 bindings) ~then ~else)
          ~else))
     then)))

(defmacro when-nonnil-let* [bindings & then]
  `(if-nonnil-let* ~bindings (do ~@then)))

(defn zip [& args]
  (apply map vector args))

(defn enumerate
  ([init xs]
   (zip (iterate inc' init) xs))
  
  ([xs]
   (enumerate 0 xs)))
