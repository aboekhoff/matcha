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
(def HASH_CODE    (with-meta 'computed_hash_code
                    {:unsynchronized-mutable true}))

;;;; helpers

(defn constructor? [x]
  (and (symbol? x) (uppercase? (first (name x)))))

(defn predicate? [x]
  (and (symbol? x) (= \? (last (name x)))))

(defn variable? [x]
  (and (symbol? x)
       (not (constructor? x))
       (not (predicate? x))))

(defn fieldname [i] (symbol (str FIELD_PREFIX i)))

(defn fieldnames [n] (map fieldname (range n)))

(defn projections [target fields]
  (for [f fields] `(. ~target ~f)))

(def metakeys {:javatype ::javatype
               :ADT      ::ADT})

(defn type-metadata [target]
  (let [target* (meta target)]
    {:ADT      (::ADT      target*)
     :javatype (::javatype target*)
     :arity    (::arity    target*)}))

;;;; convert all constructor signatures
;;;; into a vector of [constructor fields]

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
        cname (with-meta cname {::ADT true
                                ::arity 0})]
    `(do (def ~cname (new ~javatype ~tag ~@nils -1 nil))
         (alter-meta! (var ~cname) assoc ::javatype ~javatype))))

(defn- gen-constructor*
  [javatype cname tag actual-arity fields]
  (let [arity (count fields)
        meta  {:arglists (list (vec fields))
               ::arity arity
               ::ADT true}
        args  (for [_ fields] (gensym))
        args* (concat args (repeat (- actual-arity arity) nil))
        cname (with-meta cname meta)
        func  `(lambda [~@args]
                 ~@(gen-assertions fields args)
                 (new ~javatype ~tag ~@args* -1 nil))]
    `(do (intern *ns* (quote ~cname) ~func)
         (alter-meta! (var ~cname) assoc ::javatype ~javatype))))

(defn- gen-constructor [j c t a f]
  (if (empty? f)
    (gen-nullary j c t a)
    (gen-constructor* j c t a f)))

(defn- gen-to-string [tagmap]
  (let [this   'this
        cases (for [[k v] tagmap
                    :let [n (name k)
                          p (projections this (fieldnames v))
                          p (map (fn [x] `(pr-str ~x)) p)
                          p (interpose " " (cons n p))]]
                (if (zero? v)
                  [k n] [k `(str "(" ~@p ")")]))]
    `(~'toString [~this] (case (. ~this ~TAG) ~@(apply concat cases)))))

(defn- gen-hash-string [javatype tag arity this]
  (let [fields  (fieldnames arity)
        projs   (projections this fields)
        names   (for [[f p] (map vector fields projs)]
                  `(~(str f) ":" (.hashCode ~p)))
        names   (apply concat (interpose ["::"] (cons [tag] names)))]
    `(str ~(name javatype) "::" ~@names)))

(defn- gen-hash-code* [this javatype tagmap]
  (let [tagsym 'tag
        cases   (for [[tag arity] tagmap]
                  [tag (gen-hash-string javatype tagsym arity this)])]
    `(let [~tagsym (. ~this ~TAG)]
       (.hashCode (case ~tagsym ~@(apply concat cases))))))

(defn- gen-hash-code [javatype tagmap]
  (let [this 'this
        hash `(. ~this ~HASH_CODE)]
    `(~'hashCode [~this]
        (let [code# ~hash]
           (if-not (== code# -1) code#
             (let [code# ~(gen-hash-code* this javatype tagmap)]
               (set! ~HASH_CODE code#)
               code#))))))

(defn- gen-equals* [this that arity]
  (let [fields (fieldnames arity)
        pairs  (map list
                    (projections this fields)
                    (projections that fields))]
    
    (for [[a b] pairs]
      `(= ~a ~b))))

(defn- gen-equals [javatype arity]
  (let [this 'this that 'that]
    `(~'equals [~this ~that]
       (and (= (type ~this) (type ~that))
            (= (.hashCode ~this) (.hashCode ~that))
            ~@(gen-equals* this that arity)))))

(defn- gen-type* [type javatype arity tagmap]
  (let [pred     (symbol (str type "?"))
        this     'this
        metadata 'metadata
        fields   (fieldnames arity)]
    `(do (deftype ~javatype [~TAG ~@fields ~HASH_CODE ~META]
           clojure.lang.IObj
           (~'meta [~this] (or (. ~this ~META) nil))
           (~'withMeta [~this ~metadata]
             (new ~javatype
               ~@(projections this (cons TAG fields))
               (. ~this ~HASH_CODE)
               ~metadata))
           java.lang.Object
           ~(gen-to-string tagmap)
           ~(gen-hash-code javatype tagmap)
           ~(gen-equals javatype arity))
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

