(ns matchmaker.combinators
  (:refer-clojure :exclude [seq or when])
  (:require [clojure.string :as str])
  (:use (matchmaker core parser fun)))

(define (try* parser state)
  (match (parser state)
    [(Yes a) state*] -> [(Yes a) state*]
    [(No a) _]       -> [(No a) state]))

(define (maybe parser state)
  (match (parser state)
    [(Yes a) state*] -> [(Yes (Just a)) state*]
    [(No _) _]       -> [(Yes Nothing) state]))

(define (seq* parsers state acc)
  (match parsers
    [] -> [(Yes acc) state]
    [p & ps] -> 
      (match (p state)
        [(Yes a) state*] -> (seq* ps state* (conj acc a))
        [(No a) state*]  -> [(No a) state*])))

(define (seq parsers state) (seq* parsers state []))

(define (fmap function parser state)
  (>>= parser #(return (function %)) state))

(defn lift [function]
  (fn [& parsers]
    (>>= (seq parsers) #(return (apply function %)))))

(defparser (lift* function parser)
  a <- parser
  (return (apply function a)))

(define (or* parsers state acc)
  (match parsers
    [] -> (let [errs (str/join ", " acc)
                msg (str "one of " errs)]
            [(No [msg]) state])
    [p & ps] ->
      (match (p state)
        [(Yes a) state*]     -> [(Yes a) state*]
        [(No a) (? = state)] -> (or* ps state (conj acc a))
        [(No a) state*]      -> [(No a) state*])))

(define (or parsers state) (or* parsers state []))

;;;; catenates the provided error with the received error

(defparser (when predicate message parser)
  A <- lookahead
  (if (predicate A)
    token
    (fail message)))

(define (lit literal state)
  (when (eq? literal) (pr-str literal) (try* token) state))

(define (any-of items state)
  (or (map when-eq? items) state))

;;;; need to fix pattern matcher to allow recur out of tail pos.

(define (many parser state)
  (loop [state state acc []]
    (let [[choice state* :as result] (parser state)]
      (cond
       (is-yes? choice)
       (recur state* (conj acc (choice->datum choice)))

       (is-no? choice)
       (if (= state state*)
         [(Yes acc) state*]
         result)))))

(defparser (many-1 parser)
  a <- parser
  b <- (many parser)
  (return (cons a b)))

(defparser (between open parser close)
  open
  a <- parser
  close
  (return a))

(defparser (sep-by sep parser)
  a <- (maybe parser)
  b <- (many (>> sep parser))
  (match a
    (Just a) -> (return (cons a b))
    Nothing  -> (return ())))

(defparser (sep-by-1 sep parser)
  a <- parser
  b <- (many (>> sep parser))
  (return (cons a b)))

(defparser comma-list
  (between (lit \() (sep-by (lit \,) token) (lit \))))