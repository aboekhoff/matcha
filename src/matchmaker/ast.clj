(ns matchmaker.ast
  (:refer-clojure :exclude [type]))

(defn make-node
  [tag children]
  (with-meta
    (vec children)
    {::tag tag}))

(defn match-node
  [tag object]
  (= tag (::tag (meta object))))

(defmacro defnode [tag]
  (let [cnst (symbol (name tag))
        pred (symbol (str name "?"))]
    `(do (defn ~cnst
           [& ~'children]
           (make-node ~tag ~'children))
         (defn ~pred
           [~'object]
           (match-node ~tag ~'object)))))

(defmacro defnodes [& tags]
  (when-let [[t & ts] (seq tags)]
    `(do (defnode ~t) (defnodes ~@ts))))

(defnodes
  :choice
  :type
  :equals
  :variable
  :any
  :fail
  :succeed)