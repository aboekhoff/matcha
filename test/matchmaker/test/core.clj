(ns matchmaker.test.core
  (:use [matchmaker.core] :reload)
  (:use [clojure.test]))

(define-ADT toylist (Pair car toylist?) Empty)

(deftest predicates
  (is (toylist? Empty) "Empty is a toylist")
  (is (toylist? (Pair 1 Empty)) "Pairs are toylists"))

(deftest assertions
  (is (thrown? AssertionError (Pair 1 2)) "cdr must be toylist"))

(deftest hashcodes
  (is (= (.hashCode Empty)
         (.hashCode Empty))
      "nullary hash code")
  (is (= (.hashCode (Pair 1 Empty))
         (.hashCode (Pair 1 Empty)))
      "n-ary hash code")
  (is (= (.hashCode (Pair (Pair Empty Empty) Empty))
         (.hashCode (Pair (Pair Empty Empty) Empty)))
      "nested hash code"))

(deftest equality
  (is (= Empty Empty)
      "nullary equality")
  (is (= (Pair 1 Empty) (Pair 1 Empty))
      "n-ary equality")
  (is (= (Pair (Pair {:foo :bar} Empty) Empty)
         (Pair (Pair {:foo :bar} Empty) Empty))
      "nested equality"))
