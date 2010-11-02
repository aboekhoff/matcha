(ns matchmaker.parser)

;;;; our monad should

(define parse-comprehension
  [a]           -> a
  [a '<- b & c] -> `(>>= ~b (fn [~a] ~(parse-comprehension c)))
  [a & b]       -> `(>>= ~a (fn [_#] ~(parse-comprehension b))))

(defmacro doparser [& steps]
  (parse-comprehension steps))

(define-ADT Choice (Yes a) (No b))

(define (>>= parser cont state)
  (let [[result state*] (parser state)]
    (match result
      (Yes a) -> (cont a state*)
      (No b)  -> [(No b) state*])))

(define (return value state) [value state])
(define (get-state state) [state state])
(define (set-state new-state state) [nil new-state])

(define base-state
  {:tokens []
   :data   {}})








