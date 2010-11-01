(ns matchmaker.misc
  (require [clojure.pprint :as pp]))

(defn call [f & xs] (apply f xs))

(defn foldr [f xs x]
  (reduce (fn [x y] (f y x)) x (reverse xs)))

(defn unzip [xs]
  [(map first xs) (map rest xs)])

(defn map-keys [f m]
  (into {} (for [[k v] m] [(f k) v])))

(defn pipeline* [as steps]
  (if-let [[a b & more] (seq steps)]
    `(let [~as (if ~a ~b ~as)]
       ~(pipeline* as more))
    as))

(defmacro pipeline
  "pipes an expression through a series of transformations"
  [[as init] & steps]
  `(let [~as ~init] ~(pipeline* as steps)))

(defn prn-body [body]
  (case (count body)
    0 "nil"
    1 (with-out-str (pp/pprint (first body)))
      (with-out-str (pp/pprint (cons 'do body)))))

(defn pop-when [pred form]
  (if (pred form) [(first form) (rest form)] [nil form]))

(def pop-docstring (partial pop-when (comp string? first)))

(def pop-metamap (partial pop-when (comp map? first)))

(defn pop-metadata [form]
  (let [[doc form]  (pop-docstring form)
        [meta form] (pop-metamap form)
        base        (if doc {:doc doc} {})]
    [(merge base meta) form]))

(def lowers (map char (range 97 123)))
(def lowercase? (set lowers))
(def uppers (map char (range 65 91)))
(def uppercase? (set uppers))
(def letters (vec (concat lowers uppers)))
(def num-letters 52)
(defn nth-letter [n] (letters n))
(defn capitalize [s]
  (apply str (cons (.toUpperCase (str (first s))) (rest s))))


(defn raise! [& msg]
  (throw (Exception.
          (binding [*print-length* 10]
            (apply str msg)))))

(defn ordinalize [n]
  (if (zero? n)
    (raise! "can't ordinalize zero")
    (let [prefix (when (neg? n) "negative ")
          suffix (case (last (str n))
                   \1 "st"
                   \2 "nd"
                   \3 "rd")]
      (str prefix n suffix))))

(def o comp)

(def << partial)

