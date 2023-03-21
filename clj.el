(defun clj.map (f args)
  (mapcar f args))

(defun clj--normalize-arg (arg)
  (if (eq '& arg)
      '&rest
    arg))

(defmacro clj.fn (args &rest body)
  `(lambda ,(mapcar #'clj--normalize-arg args) ,@body))
;; (clj.fn [x] x), (fn [x & xs] xs)

(defmacro clj.defn (name args &rest body)
  `(defun ,name ,(mapcar #'clj--normalize-arg args) ,@body))

(defmacro clj.let (bindings &rest body)
  `(let* ,(seq-partition (mapcar #'identity bindings) 2) ,@body))
;; (let [x 1] x)

(defn clj.inc [x]
  (+ x 1))
;; (inc 1)
;; (map #'inc [1 2 3])

(defn clj.first [xs]
  (car xs))

;; (first '(1 2 3))

(defn clj.second [xs]
  (cadr xs))
;; (second '(1 2 3))

(defn clj.rest [xs]
  (cdr xs))
;; (clj.rest '(1 2 3))

(defn clj.= [& xs]
  (clj.let [l (length xs)]
           (if (= 1 l) t
             (if (= 2 l) (eq (clj.first xs) (clj.second xs))
               (and (eq (first xs) (second xs))
                    (apply #'clj.= (second xs) (cddr xs)))))))
;; (clj.= 1 1)
;; (clj.= 1 1 1)
;; (clj.= 1 1 1 1)
;; (clj.= 1 1 1 1 2)

(provide 'clj)
