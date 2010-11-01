(ns matchmaker.curry
  (:require [clojure.pprint :as pp]
            [matchmaker.misc :as misc]))

;;;; love, indian cuisine, and functional programming

(defn curry*
  [args body]
  (if (empty? args)
    body
    `(fn ~@(for [n (range 1 (inc (count args)))]
             `([~@(take n args)] ~(curry* (drop n args) body))))))

(defn curry [args body]
  (let [func  (gensym)
        body* `(fn [~@args] ~@body)
        call  `(~func ~@args)]
    `(let [~func ~body*] ~(curry* args call))))

(defmacro lambda
  "creates a curried lambda abstraction that can be applied to any
   number of arguments until saturated"
  [args & body]
  (curry args body))

(defmacro deflambda
  "defines a curried lambda astraction"
  [name & body]
  (let [meta0 (meta name)
        [meta [args & body]] (misc/pop-metadata body)
        arglist (list 'quote (interpose '-> args))
        doc     (misc/prn-body body)
        meta    (merge {:arglists arglist :doc doc} meta0 meta)
        name*   (with-meta name meta)
        saturated-name (gensym (str "saturated-" name))
        saturated-call `(~saturated-name ~@args)]
    `(do (declare ~name*)
         (defn- ~saturated-name [~@args] ~@body)
         (def ~name* ~(curry* args saturated-call)))))

