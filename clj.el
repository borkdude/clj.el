(defun map (f args)
  (mapcar f args))

(defmacro fn (args &rest body)
  `(lambda ,(mapcar #'identity args) ,@body))

(defmacro defn (name args &rest body)
  `(defun ,name ,(mapcar #'identity args) ,@body))

(defn inc [x]
  (+ x 1))

;; (inc 1)
;; (map #'inc [1 2 3])

(provide 'clj)
