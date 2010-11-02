(ns matchmaker.parser
  (:use matchmaker.core))

(define parse-comprehension
  [a]           -> a
  [a '<- b & c] -> `(>>= ~b (fn [~a] ~(parse-comprehension c)))
  [a & b]       -> `(>>= ~a (fn [_#] ~(parse-comprehension b))))

(defmacro parsing [& steps]
  (parse-comprehension steps))

(defmacro defparser [name & steps]
  (if (seq? name)
    `(define (~(first name) ~@(rest name) ~'parser-state)
       ((parsing ~@steps) ~'parser-state))
    `(define (~name ~'parser-state)
       ((parsing ~@steps) ~'parser-state))))

(define-ADT Choice (Yes a) (No b))

(define (choice->datum choice) (. choice structural_field_0))
(define (is-no? choice) (= :No (. choice structural_tag)))
(define (is-yes? choice) (= :Yes (. choice structural_tag)))

(define-ADT Maybe (Just a) Nothing)

(define (just->datum Just) (. Just structural_field_0))

(define (>>= parser cont state)
  (match (parser state)
    [(Yes a) state*] -> ((cont a) state*)
    [(No b) state*]  -> [(No b) state*]))

(define (>> discard parser state)
  (>>= discard (fn [_] parser) state))

(define (return value state)     [(Yes value) state])
(define (fail value state)       [(No value) state])
(define (get-state state)        [(Yes state) state])
(define (set-state state* state) [(Yes nil) state*])

(defn run-parser [parser tokens & {:keys [data updater]}]
  (parser {:tokens  (seq tokens)
           :data    (or data {})
           :updater (fn [token data] data)}))

(define (unit state) [(Yes Nothing) state])

(define (token state)
  (let [{:keys [tokens data updater]} state]
    (match tokens
      [x & xs] -> [(Yes x) {:tokens xs
                            :data    (updater token data)
                            :updater updater}]
      [] -> [(No "unexpected end of tokens") state])))

(define (lookahead state)
  (match (:tokens state)
    [x & xs] -> [(Yes x) state]
    []       -> [(No "unexpected end of tokens") state]))

(define (eof state)
  (match (:tokens state)
    []       -> [(Yes ::eof) state]
    [x & xs] -> [(No ["tokens remaining"]) state]))

(define (with-error err parser state)
  (match (parser state)
    [(Yes a) state*] -> [(Yes a) state*]
    [(No a) state*]  -> [(No (str err a)) state*]))

;;;; that should cover the essentials









(define t0
  (parsing
   a <- get-state
   b <- (set-state "foo")
   c <- get-state
   (return [a b c])))

(define t1
  (parsing
   a <- get-state
   b <- (set-state "foo")
   (fail "no more for you!")
   c <- get-state
   (return [a b c])))









