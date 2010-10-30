(ns matchmaker.types
  (:refer-clojure :exclude [accessor])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [clojure.pprint :as pp])
  (:use [matchmaker.misc :only [foldr call nth-letter letters]]
        [matchmaker.curry :only [lambda deflambda]]))

;;;; pass at structural types

(def FIELD_PREFIX "index_")

(def CONSTRUCTOR_PREFIX "Structural")

(defn accessor [i] (symbol (str PREFIX i)))

(defn getter [arg] (fn [n i] `(defn ~n [~arg] (. ~arg ~i))))

(defn munge-name [n] (symbol (str CONSTRUCTOR_PREFIX n)))

(defn gen-toString [name fields]
  (let [this 'this
        fields (for [f fields] `(. ~this ~f))]
    `(toString [~this]
      (str "(" (str/join " " [(quote ~name) ~@fields]) ")"))))

(defn gen-print-method [type]
  `(defmethod print-method ~type [t# w#] (.write w# (.toString t#))))

(defn gen-typedef [name fields]
  (let [tname   (munge-name name)
        pred    (symbol (str name "?"))
        aliases (map accessor (range (count fields)))
        getters (map (getter name) fields aliases)]
    `(do (deftype ~tname [~@aliases]
           ~'Object
           ~(gen-toString name aliases))
         ~(gen-print-method tname)
         (deflambda ~name [~@fields] (new ~tname ~@fields))
         (defn ~pred [~'object] (instance? ~tname ~'object))
         ~@getters)))

(defmacro define-struct [name & fields]
  (gen-typedef name fields))

;;;; ok, now lets make a naive pattern matcher that only
;;;; understands literals, variables, and structs
;;;; in order to implement the pattern compiler without losing more hair

(declare parse-n parse-n*)

(def fail ::match-failure)

(defn nth-field [t i] `(. ~t ~(accessor i)))

(defn ->pred [c] (symbol (str c "?")))

(defn parse-1 [target pat]
  (cond
   (seq? pat)    (parse-n target pat)
   (symbol? pat) (fn [more] `(let [~pat ~target] ~more))
   :else         (fn [more] `(if (= ~target ~pat) ~more ~fail))))

(defn parse-n [target [constructor & pats]]
  (fn [more]
    `(if (~(->pred constructor) ~target)
       ~((parse-n* target pats 0) more)
       ~fail)))

(defn parse-n* [target pats index]
  (if-let [[p & ps] (seq pats)]
    (let [ps* (parse-n* target ps (inc index))
          v   (gensym)
          p*  (parse-1 v p)]
      (fn [more]
        `(let [~v ~(nth-field target index)]
           ~(p* (ps* more)))))
    identity))

(defn link-patterns [cases cont]
  (foldr call cases cont))

(defn fat-bar
  "named after the [] denotation often encountered in papers"
  ([a]   (fn [b] (fat-bar a b)))
  ([a b] `(let [r# ~a] (if (= r# ::match-failure) ~b r#))))

(defn parse-patterns* [cases err]
  (fn [target]
    (let [cases* (for [[pattern action] cases]
                   (fat-bar ((parse-1 target pattern) action)))]
      (link-patterns cases* err))))

(defn gen-failure-case [cases target]
  (let [lines (for [[c a] cases] (str c " -> " a))
        body  (str/join "\n" lines)
        value `(binding [*print-length* 10] (pr-str ~target))]
    `(throw (Exception.
             (str "no matching clause for: " ~value
                  " among patterns:\n" ~body)))))

(defn parse-patterns [target cases]
  (let [cases* (partition 2 cases)
        err    (gen-failure-case cases* target)]
    ((parse-patterns* cases* err) target)))

;;;; match functions

(defn eq [x] (fn [y] (= x y)))
(defn not-eq [x] (fn [y] (not= x y)))

(defn pop-cases* [body]
  (let [[args [_ expr & more :as more*]] (split-with (not-eq '->) body)]
    (if (and (seq args) (seq more*))
      (cons [args expr] (pop-cases* more))
      [args])))

(defn pop-cases [body]
  (let [body* (pop-cases* body)]
    [(butlast body*) (last body*)]))

(defn parse-suffix [[_ & bindings :as suffix]]
  (cond
   (empty? suffix) identity
   (= :where _)    (fn [body] `(let [~@bindings] ~body))
   :else           (throw (Exception. (str "parse error on "
                                           (pr-str suffix))))))

(defn parse-body [body]
  (let [[cases suffix] (pop-cases body)
        modifier       (parse-suffix suffix)]
    [cases modifier]))

(defn check [patlists]
  (let [counts (map count patlists)]
    (when-not (apply = counts)
      (throw (Exception. (str "mismatched argument counts in "
                              (pr-str patlists)))))
    (let [gensyms (for [_ (first patlists)] (gensym))
          pretty  (take (count gensyms) letters)
          pretty  (map #(-> % str symbol) pretty)]
      [gensyms pretty])))

(defn prime [argsyms tuple action]
  (link-patterns (map parse-1 argsyms tuple) action))

(defn compile-match-fn* [body]
  (let [[cases modifier] (parse-body body)
        patterns         (map first cases)
        actions          (map second cases)
        [args args*]     (check patterns)
        primed           (map #(prime args %1 %2) patterns actions)
        err              `(throw (Exception. "pattern match failure"))
        armed            (foldr fat-bar primed err)]
    `[~args* [~@args] ~(modifier armed)]))

(defn compile-match-fn [name body]
  (let [[pretty args body] (compile-match-fn* body)
        name* (with-meta name {:arglists (list 'quote pretty)})]
    `(deflambda ~name* ~args ~body)))

;;;;

(defmacro match
  [target & cases]
  (parse-patterns target cases))

(defmacro define
  ([name value] `(def ~name ~value))
  ([name body & body*] (parse-match-fn name (cons body body*))))

(def cases
  '[[(Pair a (Pair b c)) (+ a b c)]
    [(Pair (Pair a b) c) (* a b c)]])

(def f0
     '[(Pair (Pair a b) c) -> (+ a b c)
       (Pair a (Pair b c)) -> (* a b c)])

(define-struct Pair car cdr)

(define add-pair
  (Pair a b) (Pair c d) -> (Pair (f a c) (g b d))
  :where
  f *
  g +)

(define-struct leaf value)
(define-struct node left elt right)

(define insert
  nil      x -> (leaf x)
  (leaf a) x -> (cond (or (nil? a) (= a x)) (leaf a)
                      (< x a) (node (leaf x) a nil)
                      (< a x) (node (leaf a) x nil))
  
  (node a b c) x -> (cond (< x b) (node (insert a x) b c)
                          (= x b) (node a b c)
                          (> x b) (node a b (insert c x))))

(define member?
  nil _          -> false
  (leaf a) x     -> (= a x)
  (node a b c) x -> (cond (< x b) (member? a x)
                          (> x b) (member? c x)
                          :else   true))

;;;; nullary constructors are actually pretty useful
;;;; perhaps its better to use a single type for a collection of
;;;; and add type tags
;;;; then just create unique instances without helper functions
;;;; for nullary constructors

;;;; centralizing the type to a single class would also make them
;;;; easier to use with protocols,

;;;; a richer representation would be to create an IStrucuturalType
;;;; it would contain its unique Type tag and its set of
;;;; Type-Constructor tags
;;;; all type instances would contain the parent Type tag, and their
;;;; unique Type-Constructor tag



