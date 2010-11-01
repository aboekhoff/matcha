(ns matchmaker.core
  (:require [matchmaker.curry :as curry])
  (:require [matchmaker.types :as types])
  (:require [matchmaker.match :as match])
  (:require [matchmaker.misc  :as misc]))

(defmacro match [target & cases]
  (let [target* (gensym "target")]
    `(let [~target* ~target]
       ~(match/compile-pattern-matcher 1 [target*] cases))))

(defmacro fun [& cases]
  (let [args (repeatedly (match/estimate-arity cases) gensym)]
    `(curry/lambda [~@args]
       ~(match/compile-pattern-matcher nil args cases))))


(defmacro define [name & form]
  "three methods of use:
   define foo bar     => def foo bar
   define (foo x) ... => (def foo (lambda x y z) ...)
   define foo <pat> -> <fender> ... => (def foo (fun ...))
 "
  (cond
   (seq? name)
   `(curry/deflambda ~(first name) ~(rest name) ~@form)
   
   (not (symbol? name))
   (misc/raise! "first argument to define must be a symbol or a list")

   (< (count form) 2)  `(def ~name ~@form)
   (>= (count form) 2) `(def ~name (fun ~@form))))

(defmacro define-ADT [name & specs]
  `(types/define-type ~name ~@specs))

