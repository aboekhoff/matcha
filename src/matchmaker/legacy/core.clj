(ns matchmaker.core
  (:require [clojure.set :as set]))

;;;; GOAL
;;;; implement a simple pattern compiler
;;;; on structural types as in ML or Scheme

(defn joker? [x] (= x '_))
(defn constructor? [x]
  (and (symbol? x)
       (re-find #"^[A-Z].*" (name x))))

;;;; IR

(defn constructor [c xs] [::constructor c xs])
(defn singleton   [s]    [::singleton s])
(defn variable    [v]    [::variable v])
(defn string      [s]    [::string s])
(defn number      [n]    [::number n])
(def  truth              [::true])
(def  falsehood          [::false])
(def  joker              [::joker])

;;;; parse terms

(declare pattern->tree*)

(defn pattern->tree [p]
  (cond
   (joker? p)       joker
   (constructor? p) (singleton p)
   (symbol? p)      (variable p)
   (string? p)      (string p)
   (number? p)      (number p)
   (true? p)        truth
   (false? p)       falsehood
   (seq? p)         (pattern->tree* p)))

(defn pattern->tree* [[c & xs]]
  (constructor c (vec (map pattern->tree xs))))

;;;; add variable path maps

(defn indexed [coll] (map vector (range) coll))

(defn repeated-variable-error! [variable pattern]
  (throw (Exception.
          (str "Syntax Error: variable '" variable
               "' occurs multiple times in pattern '"
               pattern "'"))))

(defn tree->chart [pattern original]
  (let [charted (atom {})
        mark!   (fn [variable path]
                  (if (@charted variable)
                    (repeated-variable-error! variable original)
                    (swap! charted assoc variable path)))
        chart!  (fn chart! [[tag value body] loc]
                  (condp = tag
                    ::variable    (mark! value (conj loc 1))
                    ::constructor (doseq [[i p] (indexed body)]
                                    (chart! p (conj loc 2 i)))
                    nil))]
    (chart! pattern [])
    @charted))

(defn chart->helpers [parsed charted]
  (let [variables (keys charted)
        locations (vals charted)
        reducer   (fn [term [path value]] (assoc-in term path value))
        rewriter  (fn [newvals] (reduce reducer parsed newvals))]
    [parsed variables locations rewriter]))

(defn pattern->helpers [pattern]
  (let [tree  (pattern->tree pattern)
        chart (tree->chart tree pattern)]
    (chart->helpers tree chart)))

(defn normalize-patterns [patterns]
  (let [helpers   (map pattern->helpers patterns)
        variables (map second helpers)
        locations (set (mapcat #(nth % 2) helpers))
        rewriters (map #(nth % 3) helpers)
        replacer  (zipmap locations (repeatedly gensym))]
    (prn replacer)
    (for [r rewriters] (r replacer))))





;;;; going to bed now

;;;; next step is to find the set of all variable locations
;;;; and create a corresponding set of gensyms

;;;; then create a function that maps the gensyms back to the original
;;;; bindings for each pattern

;;;; once all the patterns share the same free variables,
;;;; collapse identical nodes into each other to simplify the tree

;;;; finally transform the simplified structure into a case analysis

;;;; the initial compiler will be immediately usable in calliope and poet,
;;;; and should be useful in clojure as well with a macro for defining
;;;; structural types
