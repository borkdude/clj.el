(defun map (f args)
  (mapcar f args))

(defmacro fn (args &rest body)
  `(lambda ,(mapcar #'identity args) ,@body))

(defmacro defn (name args &rest body)
  `(defun ,name ,(mapcar #'identity args) ,@body))

(defmacro let (bindings &rest body)
  `(let* ,(seq-partition (mapcar #'identity bindings) 2) ,@body))
;; (let [x 1] x)

(defn inc [x]
  (+ x 1))
;; (inc 1)
;; (map #'inc [1 2 3])

(provide 'clj)
