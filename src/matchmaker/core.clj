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

;;;; now the fun begins
;;;; we can extend the matcher using the matcher

(define (match-predicate args param yes no)
  (match args
    [p]   -> (match/choose `(~p ~param) yes no)
    [p v] -> (match/choose `(~p ~param) `(let [~v ~param] ~yes) no)))

(define (match-as args param yes no)
  (match args
    [a b] -> `(let [~a ~param]
                ~(match/match-pattern param b yes no))
    [a b & c] -> (match-as [a (cons b c)] param yes no)))

(define (match-and args param yes no)
  (match args
    []       -> yes
    [x & xs] -> (let [yes* (match-and xs param yes no)]
                  (match/match-pattern x param yes* no))))

(define (match-or args param yes no)
  (match args
    [] -> no
    [x & xs] -> (let [no* (match-or xs param yes no)]
                  (match/match-pattern x param yes no*))))

(define (match-spy args param yes no)
  (match args
    [x] -> `(do (prn ~param)
                ~(match/match-pattern param x yes no))))

(defmethod match/match-special :?
  [_] match-predicate)

(defmethod match/match-special :as
  [_] match-as)

(defmethod match/match-special :or
  [_] match-or)

(defmethod match/match-special :and
  [_] match-and)

(defmethod match/match-special :spy
  [_] match-spy)

;;;; and maybe a more useful one
;;;; an :as pattern



