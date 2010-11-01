(ns matchmaker.monad
  (:refer-clojure :exclude [get update-in])
  (:require [clojure.core :as core])
  (:use matchmaker.core))

(define (parse* expr bind zero)
  (let [parse** (fn [e] (parse* e bind zero))]
    (match expr
      [a]           -> a
      [:let a & b]  -> `(let ~a ~(parse** b))
      [:when a & b] -> `(if ~a ~(parse** b) ~zero)
      [a '<- b & c] -> `(~bind ~b (fn [~a] ~(parse** c)))
      [a & b]       -> `(~bind ~a (fn [_#] ~(parse** b))))))

(define (parse expr M)
  (let [bind (gensym "bind")
        zero (gensym "zero")]
    `(let [dict# (:monad (meta ~M))
           ~bind (:bind dict#)
           ~zero (:zero dict#)]
       ~(parse* expr bind zero))))

(defmacro domonad [M & steps]
  ~(parse ))

(define e0
  '[a <- (foo)
    b <- (bar)
    (+ a b)])

;;;; how about this?

(define-ADT Typeable
  (Typed a)
  (Untyped a))

(define return Untyped)

(defmacro define-monad [name & specs]
  (let [[types fns] (split-with (complement keyword?) specs)
        fnmap (apply hash-map fns)]
    `(do (define-ADT ~name ~@types)
         (alter-meta! (var ~name) assoc :monad ~fnmap)
         (defmacro ))))

(define-monad Maybe
  Nothing
  (Just a)
  :bind (fun (Just a) f -> (f a)
             Nothing _  -> Nothing)
  :zero Nothing)

;;;; can we do this with protocols?
;;;; we don't need generic dispatch though
;;;; we just need to inject the correct type



(define (get state) [state state])

(define (put new-state state)
  [Nothing state])



