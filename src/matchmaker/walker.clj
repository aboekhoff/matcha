(ns matchmaker.walker)

(def body-form? '#{let* letfn* case* do loop* if})

(defn has-body? [form]
  (and (seq? form) (body-form? (first form))))

(defn mex* [form]
  (let [form* (macroexpand form)]
    (if-not (= form form*)
      (recur form*))))

(declare descend descend-into-body)

(defn touch-tails [form f]
  (let [form (mex* form)]
    (if (has-body? form)
      (descend-into form f)
      (f form))))

;;;; can't pass through 'loop forms
;;;; so just abort

(defn descend [[form & body]]
  (case form
    'loop 
    'do (if (empty? body) )))