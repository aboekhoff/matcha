(ns matchmaker.match
  (:use [matchmaker.misc :only
         [call foldr raise! << o ordinalize letters]]
        [matchmaker.curry :only [lambda deflambda]]
        [matchmaker.types :only
         [variable? predicate? constructor? TAG projections
          fieldname fieldnames type-metadata define-type]]
        [clojure.pprint :only [pprint]]))

;;;; ok, now lets make a naive pattern matcher that only
;;;; understands literals, variables, and structs
;;;; in order to implement the pattern compiler without losing more hair

(declare match-pattern match-special match-n-ary
         match-nullary match-list match-sequential
         match-field match-nth
         match-fields match-indexed
         match-projection match-projections)

(deflambda eq? [x y] (= x y))

(deflambda not-eq? [x y] (not= x y))

(defn choose [test yes no]
  `(if ~test ~yes ~no))

(defn nth-field [t i] `(. ~t ~(fieldname i)))

(defn nth-index [t i] `(nth ~t ~i))

(defn indexed [xs] (map vector (range) xs))

(defn malformed-error [p]
  (raise! "malformed pattern: " p))

(defn project [expr wants-target]
  (let [g (gensym)]
    `(let [~g ~expr]
       ~(wants-target g))))

(defn check-arity! [arity actual cnst & [form]]
  (when-not (= arity actual)
    (raise! "invalid constructor sig:\n"
            cnst " takes " arity " arguments but got " actual
            (when form (str "\nin " form)))))

(defn typeinfo [sym]
  (let [tag  (keyword sym)
        data (type-metadata (resolve sym))]
    (when-not (:ADT data)
      (raise! sym " is not an ADT cnst"))
    (assoc data :tag tag)))

(defn check-type [t javatype tag]
  `(and (instance? ~t ~javatype)
        (= (. ~t ~TAG) ~tag)))

(defn parse-vector [v]
  (let [[xs ys] (split-with (not-eq? '&) v)
        y       (second ys)]
    [xs (if y `>= `=) (count xs) y]))

(deflambda match-literal [t p yes no]
  (choose `(= ~t ~p) yes no))

(deflambda match-pattern [t p yes no]
  (condp call p
    predicate?   (choose `(~p ~t) yes no)
    variable?    `(let [~p ~t] ~yes)
    constructor? (match-nullary t p yes no)
    vector?      (match-sequential t p yes no)
    seq?         (match-list t p yes no)
                 (match-literal t p yes no)))

(deflambda match-list [t p yes no]
  (let [[head & tail] p]
    (condp call head
      (eq? 'quote) (match-literal t p yes no)
      constructor? (if (empty? tail)
                     (match-nullary t head yes no)
                     (match-n-ary t head tail yes no))
      predicate?   (if-not (= 1 (count tail))
                     (malformed-error p)
                     (choose
                      `(~head ~t)
                      `(let [~(first tail) ~t] ~yes)
                      no))
      keyword?     (match-special t head tail yes no)
      (malformed-error p))))

(deflambda match-nullary [t c yes no]
  (let [{:keys [javatype tag arity]} (typeinfo c)]
    (check-arity! arity 0 c)
    (choose (check-type t javatype tag) yes no)))

(deflambda match-n-ary [t c ps yes no]
  (let [{:keys [javatype tag arity]} (typeinfo c)]
    (check-arity! arity (count ps) c (cons c ps))
    (choose
     (check-type t javatype tag)
     (match-fields t ps yes no)
     no)))

(deflambda match-rest [t p i yes no]
  (project `(nthnext ~t ~i) #(match-pattern % p yes no)))

(deflambda match-sequential [t p yes no]
  (let [[ps op len restpat] (parse-vector p)
        ps* (match-indexed t ps)]
    (choose
     `(and (sequential? ~t) (~op ~len (count ~t)))
     (if restpat
       (ps* (match-rest t restpat len yes no) no)
       (ps* yes no))
     no)))

(deflambda match-projection [accessor index t p yes no]
  (project (accessor t index) #(match-pattern % p yes no)))

(deflambda match-projections [accessor t ps yes no]
  (let [projs (for [[idx p] (indexed ps)]
                (match-projection accessor idx t p))]
    (foldr #(%1 %2 no) projs yes)))

(def match-indexed (match-projections nth-index))

(def match-fields (match-projections nth-field))

;;;; so far so good
;;;; now we need to parse the initial cases
;;;; functions are really no different, we just fold across
;;;; the arguments before linking the cases

(declare parse-ps parse-fender parse-guards parse-case parse-cases)

(def delim?  '#{-> |})
(def arrow?  (eq? '->))
(def bar?    (eq? '|))
(defn pat? [x] (not (delim? x))) ;; loose
(defn where? [x] (and (seq? x) (= 'where (first x))))

(defn err
  ([] (err "pattern syntax error"))
  ([e] (raise! e)))

(defn err-when [e] (when e (err)))

(defn <-pat [xs]
  (err-when (empty? xs))
  (let [[y & ys] (seq xs)]
    (if (pat? y)
      (let [[z zs] (<-pat ys)]
        [(cons y z) zs])
      [() xs])))

(defn <-action [xs]
  (when (and (> (count xs) 1)
             (arrow? (first xs)))
    [{:action (second xs)} (drop 2 xs)]))

(defn <-guards [xs]
  (if (and (> (count xs) 2)
           (bar? (first xs)))
    (let [[_ x y & more] xs
          [z zs] (<-guards more)]
      [(cons [x y] z) zs])
    [nil xs]))

(defn <-fender [xs]
  (or (<-action xs)
      (let [[ys zs] (<-guards xs)]
        (err-when (empty? ys))
        [{:guard ys} zs])))

(defn <-case [xs]
  (let [[a xs] (<-pat xs)
        [b xs] (<-fender xs)]
    (err-when (empty? a))
    [(merge {:pattern a} b) xs]))

(defn <-cases [xs]
  (let [[a xs] (<-case xs)]
    (if (> (count xs) 2)
      (let [[b xs] (<-case xs)]
        [(cons a (list b)) xs])
      [a xs])))

(defn <-where [xs]
  (if (and (= 1 (count xs))
           (= 'where (first xs)))
    [(rest (first xs)) xs]
    [nil xs]))

(defn compile-action [idx action]
  [{idx action} `(recur true ~idx) `(recur false ~(inc idx))])

(deflambda subindex [a b]
  (keyword (str a (str (letters b)))))

(defn compile-guard [idx guard]
  (let [tsts (map first guard)
        acts (map second guard) 
        idxs (range (count guard))
        ks   (map subindex (repeat idx) idxs)
        jmps (for [k ks] `(recur true ~k))
        fail `(recur false ~(inc idx))
        tail `(cond ~@(interleave tsts jmps) ~fail)]
    [(zipmap ks acts) tail]))

(deflambda normalize-patterns [ps yes no ts]
  (let [ps* (map #(match-pattern %1 %2 yes) ts ps)]
    (foldr call ps* no)))

(defn normalize-case [])




(defn compile-1 [idx {:keys [pattern action guard]}]
  
  ())

(defn parse [xs]
  (let [[cases xs] (<-cases xs)
        [where xs] (<-where xs)]
    (err-when (seq xs))
    {:cases (indexed cases) :where where}))

(defn prepare-pattern [idx pats]
  (err-when (not (apply = (map count pats))))
  )

(defn process [{:keys [cases where]}]
  ())



;;;;

(deflambda build-matcher [t p i]
  (match-pattern t p `(recur true ~i) `(recur false (inc ~i))))

(deflambda build-action [action idx]
  {(keyword (str idx)) action})



(deflambda build-guard [test x y]
  `(if ~test (recur true ~x) ~y))

(deflambda build-guards* [i gs]
  (for [[i* [t a]] (indexed gs)]
    (let [k (subindex i i*)
          t (build-guard t k)
          a {k a}]
      [t a])))

(deflambda build-guards [i gs]
  (let [pairs (build-guards* i gs)
        fns   (map first pairs)
        cases (apply merge (map second pairs))
        fail  `(recur false ~(inc i))
        tail  (foldr call fns fail)]
    [tail cases]))






(defn build-dispatch [xs]
  (let [pairs (parse-cases xs)]))

(comment
  (defn parse-guards [acc [x y z & more :as all]]
    (cond
     (bar? x) (recur (conj acc [y z]) more)
     :else    [[:GUARD (vec (apply concat acc))] all]))

  (defn parse-case [xs]
    (let [[ps xs]     (parse-ps xs)
          [fender xs] (parse-fender xs)]
      [[ps fender] xs]))

  (defn parse-cases* [acc xs]
    (let [[case xs] (parse-case xs)
          acc       (conj acc case)]
      (if (< (count xs) 2)
        [acc xs]
        (recur acc xs))))

  (defn parse-cases [xs]
    (let [[cases rem] (parse-cases* [] xs)]
      (cond
       (empty? rem) cases
       (where? rem) [:WHERE (rest (first xs)) cases]
       :else        (syntax-error!))))

  (defn build-matcher [t p index]
    (match-pattern t p (jump index))))



(def cases0
     '[:foo -> 42
       :bar -> 21])

(comment

  (define-type pair Nil (Cons car cdr))
  
;;;;

  (defmacro match
    [t & cases]
    (parse-pterns t cases))

  (defmacro define
    ([name value] `(def ~name ~value))
    ([name body & body*] (compile-match-fn name (cons body body*))))

  (def cases
       '[[(Pair a (Pair b c)) (+ a b c)]
         [(Pair (Pair a b) c) (* a b c)]])

  (def f0
       '[(Pair (Pair a b) c) -> (+ a b c)
         (Pair a (Pair b c)) -> (* a b c)])

  (define-struct Pair car cdr)

  (define add-pair
    (Pair a b) (Pair c d) -> (Pair (f a c) (g b d))
    (where f * g *))

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



;;;; nullary cnsts are actually pretty useful
;;;; perhaps its better to use a single type for a collection of
;;;; and add type tags
;;;; then just create unique instances without helper functions
;;;; for nullary cnsts

;;;; centralizing the type to a single class would also make them
;;;; easier to use with protocols,

;;;; a richer representation would be to create an IStrucuturalType
;;;; it would yes noain its unique Type tag and its set of
;;;; Type-Cnst tags
;;;; all type instances would yes noain the parent Type tag, and their
;;;; unique Type-Cnst tag

;;;; how to do this?

;;;; (define-type (Tree a) 
;;;;   Empty
;;;;   (Leaf a)
;;;;   (Node (Tree a) (Tree a)))
;;;;
;;;; ISeq
;;;; ...

;;;; we create a deftype for the ADT
;;;; it has the fields: type cnst index_0 ... index_N
;;;; we create functions to implement each cnst
;;;; we put the type metadata on each cnst
;;;; so that the ptern matcher can get access to type metadata
;;;; for nullary cnsts, we create a single instance 
;;;; also allow implementations in the declaration

;;;; accept predicates/type variables?
;;;; I think its possible, but for the moment, lets restrict ourselves
;;;; to a usable implementation of unions without type checking
)