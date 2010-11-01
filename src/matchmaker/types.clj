(ns matchmaker.types
  (:refer-clojure :exclude [accessor])
  (:require [clojure.string :as str]
            [clojure.pprint :as pp])
  (:use matchmaker.misc
        [matchmaker.curry :only [lambda deflambda]]))

;;;; magic strings

(def FIELD_PREFIX "structural_field_")
(def TYPE_PREFIX  "Structural")
(def TAG          'structural_tag)
(def META         'metadata)

;;;; helpers

(defn variable? [x]
  (and (symbol? x) (lowercase? (first (name x)))))

(defn constructor? [x]
  (and (symbol? x) (uppercase? (first (name x)))))

(defn predicate? [x]
  (and (symbol? x) (= \? (last (name x)))))

(defn fieldname [i] (symbol (str FIELD_PREFIX i)))

(defn fieldnames [n] (map accessor (range n)))

(defn projections [target fields]
  (for [f fields] `(. ~target ~f)))

(def metakeys {:javatype ::javatype
               :ADT      ::ADT})

(defn type-metadata [target]
  (let [target* (meta target)]
    {:ADT      (::ADT      target*)
     :javatype (::javatype target*)
     :arity    (::arity    target*)}))

;;;; first figure out the max arity

(defn- normalize-spec [spec]
  (cond
   (symbol? spec) [spec '()]
   (seq? spec)    [(first spec) (rest spec)]
   :else          (raise! "invalid constructor spec: " spec)))

(defn- parse-spec [spec]
  (let [[cname fields] (normalize-spec spec)]
    (when-not (constructor? cname)
      (raise! "constructors must begin with an uppercase letter"))
    [cname (keyword cname) fields (count fields)]))

(defn- gen-assertions [preds args]
  (for [[p a] (map vector preds args)
        :when (predicate? p)]
    `(assert (~p ~a))))

(defn- gen-nullary [javatype cname tag actual-arity]
  (let [nils  (repeat actual-arity nil)
        cname (with-meta cname {::javatype javatype
                                ::ADT true
                                ::arity 0})]
    `(def ~cname (new ~javatype ~tag ~@nils nil))))

(defn- gen-constructor*
  [javatype cname tag actual-arity fields]
  (let [arity (count fields)
        meta  {:arglists (list (vec fields))
               ::arity arity
               ::javatype javatype
               ::ADT true}
        args  (for [_ fields] (gensym))
        args* (concat args (repeat (- actual-arity arity) nil))
        cname (with-meta cname meta)
        func  `(lambda [~@args]
                 ~@(gen-assertions fields args)
                 (new ~javatype ~tag ~@args* nil))]
    `(intern *ns* (quote ~cname) ~func)))

(defn- gen-constructor [j c t a f]
  (if (empty? f)
    (gen-nullary j c t a)
    (gen-constructor* j c t a f)))

(defn- gen-to-string [tagmap]
  (let [this   'this
        writer 'writer
        cases (for [[k v] tagmap
                    :let [n (name k)
                          p (projections this (fieldnames v))
                          p (interpose " " (cons n p))]]
                (if (zero? v)
                  [k n] [k `(str "(" ~@p ")")]))]
    `(~'toString [~this] (case (. ~this ~TAG) ~@(apply concat cases)))))

(defn- gen-type* [type javatype arity tagmap]
  (let [pred     (symbol (str type "?"))
        this     'this
        metadata 'metadata
        fields   (fieldnames arity)]
    `(do (deftype ~javatype [~TAG ~@fields ~META]
           clojure.lang.IObj
           (~'meta [~this] (or (. ~this ~META) nil))
           (~'withMeta [~this ~metadata]
             (new ~javatype
               ~@(projections this (cons TAG fields)) ~metadata))
           java.lang.Object
           ~(gen-to-string tagmap))
         (defn ~pred [~'object]
           (instance? ~javatype ~'object))
         (def ~type ~tagmap))))

(defn- gen-type [type specs]
  (let [javatype (symbol (str TYPE_PREFIX (capitalize (name type))))
        specs    (map parse-spec specs)
        arities  (map last specs)
        arity    (apply max arities)
        args     (list* TAG (fieldnames arity))
        tagmap   (zipmap (map second specs) arities)]
    `(do ~(gen-type* type javatype arity tagmap)
         ~@(for [[cname tag fields _] specs]
             (gen-constructor javatype cname tag arity fields))
         (defmethod print-method ~javatype
           [x# w#] (.write w# (.toString x#))))))

(defmacro define-type [type & constructors]
  (gen-type type constructors))

