; input -> (result . input) | #f
; tested in parser-combinators-stream

(defun parse (p i)
  (p i))

(defun p-unit (x)
  (fun (i) (cons x i)))

(defun p-bind (m f)
  (fun (i)
    (let1 x (m i)
      (and x ((f (car x)) (cdr x))))))

(def p-fail
  (fun (i) #f))

(defmacro p-reify body
  `(reset (p-unit (begin ,@body))))

(defun p-reflect (m)
  (shift k (p-bind m k)))

;;;;

(defmacro p-lazy (p)
  `(fun (i) (,p i)))

(defun p-map (f p)
  (p-reify
    (let1 x (p-reflect p)
      (f x))))

(defun p-where (f p)
  (fun (i)
    (let1 x (p i)
      (and x (f (car x)) x))))

(defun p-or (p q)
  (fun (i)
    (or (p i) (q i))))

(defun p-cons (p q)
  (p-reify
    (let ([x (p-reflect p)]
          [y (p-reflect q)])
      (cons x y))))

(def p-nil (p-unit ()))

(defun p-choice ps
  (foldr p-or p-fail ps))

(defun p-seq ps
  (foldr p-cons p-nil ps))

(defun p-some (p)
  (p-cons p (p-many p)))

(defun p-many (p)
  (p-lazy
    (p-choice (p-some p) p-nil)))

(defun p-reduce (f . args)
  (p-map (fun (ls) (apply f ls)) (apply p-seq args)))
