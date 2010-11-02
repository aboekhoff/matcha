(ns matchmaker.match
  (:use [matchmaker.misc :only
         [call foldr raise! << o ordinalize letters unzip map-keys]]
        [matchmaker.curry :only [lambda deflambda]]
        [matchmaker.types :only
         [variable? predicate? constructor? TAG projections
          fieldname fieldnames type-metadata define-type]]
        [clojure.pprint :only [pprint]])
  (:require [clojure.string :as str]))

;;;; ok, now lets make a naive pattern matcher that only
;;;; understands literals, variables, and structs
;;;; in order to implement the pattern compiler without losing more hair

(declare match-pattern match-special match-n-ary
         match-nullary match-list match-sequential
         match-field match-nth
         match-fields match-indexed
         match-projection match-projections)

;;;; it should be possible to implement full regular expression
;;;; parsing with this thing
;;;; damn, maybe we should keep this to ourselves for a bit
;;;; (:! ...) (:+ ...) (:* ...) (:or ...) (:as ...)

(defmulti match-special
  "extension point for adding syntax to the pattern matcher.
   dispatch value must be a symbol.
   The multifn must return a curried function of four arguments:
   1. the tail of the form 
   2. the current parameter
   3. the match-success continuation (yes).
   4. the match-failure continuation (no).
   The result of applying the returned function to the above arguments
   will be inserted in tail position."
  identity)

(defmethod match-special :default
  [_] (raise! "unknown operator"))

(deflambda eq? [x y] (= x y))

(deflambda not-eq? [x y] (not= x y))

(defn ignore? [x] (= x '_))

(defn choose [test yes no]
  `(if ~test ~yes ~no))

(defn nth-field [t i] `(. ~t ~(fieldname i)))

(defn nth-index [t i] `(nth ~t ~i))

(defn indexed [xs] (map vector (range) xs))

(defn malformed-error [p]
  (raise! "malformed pattern: " p))

(defn project [expr wants-target]
  (let [g (gensym)]
    `(let [~g ~expr]
       ~(wants-target g))))

(defn check-arity! [arity actual cnst & [form]]
  (when-not (= arity actual)
    (raise! "invalid constructor sig:\n"
            cnst " takes " arity " arguments but got " actual
            (when form (str "\nin " form)))))

(defn typeinfo [sym]
  (let [tag  (keyword sym)
        data (type-metadata (resolve sym))]
    (when-not (:ADT data)
      (raise! sym " is not an ADT constructor"))
    (assoc data :tag tag)))

(defn check-type [t javatype tag]
  `(and (instance? ~javatype ~t)
        (= (. ~t ~TAG) ~tag)))

(defn parse-vector [v]
  (let [[xs ys] (split-with (not-eq? '&) v)
        y       (second ys)]
    [xs (if y `>= `=) (count xs) y]))

(deflambda match-literal [t p yes no]
  (choose `(= ~t ~p) yes no))

(deflambda match-pattern [t p yes no]
  (condp call p
    ignore?      yes
    predicate?   (choose `(~p ~t) yes no)
    variable?    `(let [~p ~t] ~yes)
    constructor? (match-nullary t p yes no)
    vector?      (match-sequential t p yes no)
    seq?         (match-list t p yes no)
                 (match-literal t p yes no)))

(deflambda match-list [t p yes no]
  (let [[head & tail] p]
    (condp call head
      (eq? 'quote) (match-literal t p yes no)
      constructor? (if (empty? tail)
                     (match-nullary t head yes no)
                     (match-n-ary t head tail yes no))
      symbol?      (do ((match-special head) tail t yes no))
      (malformed-error p))))

(deflambda match-nullary [t c yes no]
  (let [{:keys [javatype tag arity]} (typeinfo c)]
    (check-arity! arity 0 c)
    (choose (check-type t javatype tag) yes no)))

(deflambda match-n-ary [t c ps yes no]
  (let [{:keys [javatype tag arity]} (typeinfo c)]
    (check-arity! arity (count ps) c (cons c ps))
    (choose
     (check-type t javatype tag)
     (match-fields t ps yes no)
     no)))

(deflambda match-rest [t p i yes no]
  (project `(nthnext ~t ~i) #(match-pattern % p yes no)))

(deflambda match-sequential [t p yes no]
  (let [[ps op len restpat] (parse-vector p)
        ps* (match-indexed t ps)]
    (if (empty? p)
      (choose `(empty? ~t) yes no)
      (choose `(and (sequential? ~t) (~op (count ~t) ~len))
       (if restpat
         (ps* (match-rest t restpat len yes no) no)
         (ps* yes no))
       no))))

(deflambda match-projection [accessor index t p yes no]
  (project (accessor t index) #(match-pattern % p yes no)))

(deflambda match-projections [accessor t ps yes no]
  (let [projs (for [[idx p] (indexed ps)]
                (match-projection accessor idx t p))]
    (foldr #(%1 %2 no) projs yes)))

(def match-indexed (match-projections nth-index))

(def match-fields (match-projections nth-field))

;;;; begin parser

(def delim?  '#{-> |})
(def arrow?  (eq? '->))
(def bar?    (eq? '|))
(defn pat? [x] (not (delim? x))) ;; loose
(defn where? [x] (and (seq? x) (= 'where (first x))))

(defn err
  ([] (err "pattern syntax error"))
  ([e] (raise! e)))

(defn err-when [e & msg] (when e (apply err msg)))

(defn <-pat [xs]
  (err-when (empty? xs))
  (let [[y & ys] (seq xs)]
    (if (pat? y)
      (let [[z zs] (<-pat ys)]
        [(cons y z) zs])
      [() xs])))

(defn <-action [xs]
  (when (and (> (count xs) 1)
             (arrow? (first xs)))
    [{:action (second xs)} (drop 2 xs)]))

(defn <-guards [xs]
  (if (and (> (count xs) 2)
           (bar? (first xs)))
    (let [[_ x y & more] xs
          [z zs] (<-guards more)]
      [(cons [x y] z) zs])
    [nil xs]))

(defn <-fender [xs]
  (or (<-action xs)
      (let [[ys zs] (<-guards xs)]
        (err-when (empty? ys))
        [{:guard ys} zs])))

(defn <-case [xs]
  (let [[a xs] (<-pat xs)
        [b xs] (<-fender xs)]
    (err-when (empty? a))
    [(merge {:pattern a} b) xs]))

(defn <-cases [xs]
  (let [[a xs] (<-case xs)]
    (if (> (count xs) 1)
      (let [[b xs] (<-cases xs)]
        [(cons a b) xs])
      [(list a) xs])))

(defn <-where [xs]
  (if (and (= 1 (count xs)))
    (let [[a & b] (first xs)]
      (err-when (not= 'where a))
      [b nil])
    [nil xs]))

(defn parse [xs]
  (let [[cases xs] (<-cases xs)
        [where xs] (<-where xs)]
    (err-when (seq xs))
    {:cases (indexed cases) :where where}))

;;;; end parser
;;;; begin compiler

(deflambda compile-pattern
  [ps yes no ts]
  (let [ps* (map match-pattern ts ps)]
    (foldr #(%1 %2 no) (butlast ps*) ((last ps*) yes no))))

(defn compile-yes [idx map]
  (if (contains? map :action)
    (:action map)
    `(cond ~@(apply concat (:guard map))
           :else (recur ~(inc idx)))))

(defn compile-case
  [[idx map] ts]
  (let [yes (compile-yes idx map)
        no  `(recur ~(inc idx))]
    (compile-pattern (:pattern map) yes no ts)))

(defn build-pretty-error [cases symbols]
  (let [cases* (for [[_ c] cases]
                 (str/join " " (map pr-str (:pattern c))))
        syms   (interpose " " (for [s symbols] `(pr-str ~s)))
        pats   (apply str (interpose "\n" cases*))]
    `(let [val# (str ~@syms)]
       (raise! "no matching pattern found for " val#
               " among patterns:\n" ~pats))))

(deflambda compile-cases*
  "the function most resembling an entry point into the compiler"
  [cases targets]
  (let [matchers (map compile-case cases (repeat targets))
        fail-at  (count cases)
        failure  (build-pretty-error cases targets)
        dispatch (interleave (range fail-at) matchers)
        dispatch `(loop [goto# 0]
                    (case goto# ~@dispatch ~failure))]
    dispatch))

;;;; need to inspect cases to ensure args are properly matched

(defn validate-patterns! [arity cases]
  (let [lens (map (comp count :pattern second) cases)
        test (if (= 1 arity)
               #(every? (eq? arity) %)
               #(apply = %))]
    (when-not (test lens)
      (raise! "syntax error: unbalanced pattern parameters"))))

(defn estimate-arity [cases]
  (count (take-while (o not delim?) cases)))

(deflambda compile-pattern-matcher [arity targets cases]
  (let [{:keys [cases where]} (parse cases)
        matcher (compile-cases* cases targets)]
    (validate-patterns! arity cases)
    (if where `(let [~@where] ~matcher) matcher)))


