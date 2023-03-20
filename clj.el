(defun clj-map (f args)
  (mapcar f args))

(defmacro clj-fn (args &rest body)
  `(lambda ,(mapcar #'identity args) ,@body))

(defmacro clj-defn (name args &rest body)
  `(defun ,name ,(mapcar #'identity args) ,@body))

(defmacro clj-let (bindings &rest body)
  `(let* ,(seq-partition (mapcar #'identity bindings) 2) ,@body))
;; (let [x 1] x)

(clj-defn clj-inc [x]
          (+ x 1))
;; (inc 1)
;; (map #'inc [1 2 3])

(clj-defn clj-= [x y]
          (eq x y))

(clj-defn clj-first [a-list]
          (car a-list))

(clj-defn clj-rest [a-list]
          (cdr a-list))

(clj-defn
 clj--qualify [s-expr]
 (if (listp s-expr)
     (if (memq (clj-first s-expr) '(fn inc let))
         (cons (make-symbol (concat "clj-" (symbol-name (clj-first s-expr)))) (clj-map #'clj--qualify (clj-rest s-expr)))
       (clj-map #'clj--qualify s-expr))
   (if (vectorp s-expr)
       (apply #'vector (mapcar #'clj--qualify s-expr))
     s-expr)))
;; (clj--qualify '(fn [x] x))

(defmacro clj (body)
  (clj--qualify body))

(clj (let [f (fn [x] (inc x))] (f 2)))

(macroexpand-all (clj--qualify '(let [f (fn [x] (inc x))] (f 2))))q

(provide 'clj)
