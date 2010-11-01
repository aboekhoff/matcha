(ns matchmaker.walker
  (:use matchmaker.core))

(def body-form? '#{let* letfn* case* do loop* if})

(defn has-body? [form]
  (and (seq? form) (body-form? (first form))))

(defn mex* [form]
  (let [form* (macroexpand form)]
    (if-not (= form form*)
      (recur form*)
      form*)))

(declare descend descend-into-body)

(def not-seq? (comp not seq?))

(defn walk [form edit]
  (let [form (mex* form)
        f    #(walk % edit)
        g    #(let [a (butlast %) b (last %)]
                (concat a [(f b)]))]
    (match form
      not-seq?      -> (edit form)
      ['do & x]     -> `(do ~@(g x))
      ['loop* & _]  -> form
      ['let* x & y] -> `(let* ~x ~@(g y))
      ['if x y z]   -> `(if ~x ~(f y) ~(f z))
      ['letfn* x y] -> `(letfn* ~x ~@(g y))
      ['case* & x]  -> (let [[t a b c d default dispatch] x]
                         `(case* ~t ~a ~b ~c ~d
                                 ~(f default)
                                 ~(into {} (for [[k v] dispatch]
                                             [k (f v)]))))
      otherwise     -> (edit form))))

(defn rewrite-recur [form]
  (match form
    not-seq?        -> form
    ['recur & args] -> `(rewrote-recur! ~@args)
    otherwise       -> form))

(def form0
     '(let [x 42]
        (let [y 99]
          (let [z 200]
            (if foo
              (recur (+ x y z))
              (recur (* x y z)))))))