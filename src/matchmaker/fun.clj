(ns matchmaker.fun
  (:refer-clojure :exclude [struct accessor])
  (:use [matchmaker.curry :only [deflambda lambda]]))

(deflambda foldl [f x xs]
  (reduce f x xs))

(deflambda foldr [f xs x]
  (let [xs* (reverse xs)]
    (reduce (fn [x y] (f y x)) x xs*)))

(deflambda unfoldl [f x]
  (when-let [y (f x)] (cons x (unfoldl f y))))

(deflambda unfoldr [f x]
  (reverse (unfoldl f x)))