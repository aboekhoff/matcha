(ns matchmaker.compiler)

;;;; starting point is one pattern, one action

(defn call [f & xs] (apply f xs))
(defn conj* [x xs] (if (empty? xs) x (apply conj x xs)))

(defn bool? [x]
  (or (true? x) (false? x)))

(defn inc* [path]
  (update-in path [(dec (count path))] inc))

(declare parse*)

(defn parse [proj pat acc]
  (let [lit (fn [c]
              (conj acc
                [proj [:type c]]
                [proj [:= pat]]))]
    (cond
     (symbol? pat) (conj acc [proj [:var pat]])
     (string? pat) (lit 'String)
     (number? pat) (lit 'Number)
     (bool? pat)   (lit 'Boolean)
     (seq? pat)    (parse* proj pat acc))))

(defn parse* [proj [cnst & pats] acc]
  (let [acc (conj acc [proj [:type cnst]])]
    (loop [acc acc
           idx 0
           pats pats]
      (if-let [[p & ps] (seq pats)]
        (recur (parse (conj proj idx) p acc)
               (inc idx)
               ps)
        acc))))

(defn parse-patterns [patmap]
  (into {} (for [[uid pat] patmap] [uid (parse [] pat [])])))

(defn eta-dict []
  (let [uids  (atom {})
        locs  (atom {})
        id    (atom 0)
        name! (fn [loc]
                (let [name* (symbol (str "eta_" (swap! id inc)))]
                  (swap! locs assoc loc name*)
                  name*))]
    (fn
      ([] @uids)
      ([sym loc uid]
         (let [eta-name (or (@locs loc) (name! loc))]
           (do (swap! uids assoc-in [uid sym] eta-name) eta-name))))))

(defn eta-rename1 [uid pattern dict]
  (vec
    (for [[path [tag datum] :as pat] pattern]
      (if (= :var tag)
        [path [:var (dict datum path uid)]]
        pat))))

(defn eta-rename [patmap]
  (let [dict      (eta-dict)
        patterns* (into {} (for [[uid pat] patmap]
                             [uid (eta-rename1 uid pat dict)]))]
    [patterns* (dict)]))

(defn uid-map [xs] (apply sorted-map (interleave (range) xs)))

(defn fold* [seeking alts]
  (into {}
    (for [[uid [head & tail]] alts
          :when (= seeking head)]
      [uid tail])))

(defn fold-cases [case-map]
  (let [top-id   (apply min (keys case-map))
        alts     (dissoc case-map top-id)
        [a & b]  (case-map top-id)
        similar  (fold* a alts)]

    (if (empty? similar)
      case-map
      (let [leaf     [a [:branch (assoc similar top-id b)]]
            case-map (apply dissoc case-map (keys similar))
            case-map (assoc case-map top-id leaf)]
        case-map))))

(defn compile-cases [cases]
  (let [pats       (uid-map (take-nth 2 cases))
        actions    (uid-map (take-nth 2 (rest cases)))
        pats       (parse-patterns pats)
        [pats eta] (eta-rename pats)
        f          (fn [pats [id action]]
                     (update-in pats [id] conj
                        [:action (or (eta id) {}) action]))
        pats       (reduce f pats actions)]
    (fold-cases pats)
    ))

;;     [pats actions (fold-cases pats)]

(def pats* '[(Pair z   (Pair a b))
             (Pair a   (Pair c d))
             (Pair c   (Pair e f))
             (Pair "x" (Pair x y))])

(def pats (add-uids pats*))

(def cases '["FOO"               "got foo"
             (Pair a (Pair b c)) (+ a b c)
             42                  "got 42"
             (Pair (Pair a b) c) (* a b c)
             (Pair a b)          42])








