(ns matchmaker.fun
  (:refer-clojure :exclude [struct accessor])
  (:use matchmaker.core))

(define (foldl f x xs)
  (reduce f x xs))

(define (foldr f xs x)
  (let [xs* (reverse xs)]
    (reduce (fn [x y] (f y x)) x xs*)))

(define (unfoldl f x)
  (lazy-seq
   (when-let [y (f x)] (cons x (unfoldl f y)))))

;;;; curried equals, not equals

(define (eq? x y) (= x y))

(define (not-eq? x y) (not= x y))

(define (not* p x) (not (p x)))

;;;; predicates

(defn conjoin [& predicates]
  (fn [x]
    (loop [preds predicates]
      (if (empty? preds)
        true
        (let [[p & ps] preds]
          (if (p x) (recur ps) false))))))

(defn disjoin [& predicates]
  (fn [x]
    (loop [preds predicates]
      (if (empty? preds)
        false
        (let [[p & ps] preds]
          (or (p x) (recur ps)))))))