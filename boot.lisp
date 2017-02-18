(def cons (builtin cons))
(def list (fun xs xs))

(def defun (macro (sym . body) (list 'def sym (cons 'fun body))))
(def defmacro (macro (sym . body) (list 'def sym (cons 'macro body))))
(def defbuiltin (macro (sym . intf) (list 'def sym (list 'builtin sym))))

(defbuiltin exit (exitcode))
(defbuiltin error (msg))

(defbuiltin gensym ())

(defbuiltin car (cons))
(defbuiltin cdr (cons))

(defbuiltin apply (f args))

(defun compose (f g)
  (fun (x) (f (g x))))

(defun flip (f)
  (fun (a b) (f b a)))

(def caar (compose car car))
(def cadr (compose car cdr))
(def cdar (compose cdr car))
(def cddr (compose cdr cdr))
(def caaar (compose car caar))
(def cdaar (compose cdr caar))
(def cadar (compose car cdar))
(def cddar (compose cdr cdar))
(def caadr (compose car cadr))
(def cdadr (compose cdr cadr))
(def caddr (compose car cddr))
(def cdddr (compose cdr cddr))

(defbuiltin num? (x))
(defbuiltin sym? (x))
(defbuiltin str? (x))
(defbuiltin cons? (x))
(defbuiltin nil? (x))
(defbuiltin bool? (x))
(defbuiltin proc? (x))
(defbuiltin meta? (x))

(defun list? (x)
  (if (nil? x)
    #t
    (if (cons? x)
      (list? (cdr x))
      #f)))

(defbuiltin + nums)
(defbuiltin - (num . nums))
(defbuiltin * nums)
(defbuiltin / (num . nums))
(defbuiltin % (num . nums))

(defbuiltin concat strs)
(defbuiltin length (str)) ; byte size

(defbuiltin = xs)
(defbuiltin < nums-or-strs)
(defbuiltin > nums-or-strs)
(defbuiltin <= nums-or-strs)
(defbuiltin >= nums-or-strs)

(defun map (f xs)
  (if (nil? xs)
    '()
    (cons (f (car xs)) (map f (cdr xs)))))

(defun foldl (f i xs)
  (if (nil? xs)
    i
    (foldl f (f i (car xs)) (cdr xs))))

(defun foldr (f i xs)
  (if (nil? xs)
    i
    (f (car xs) (foldr f i (cdr xs)))))

(defun append ls
  (foldr *append '() ls))

(defun *append (a b)
  (if (nil? a)
    b
    (cons (car a) (*append (cdr a) b))))

(defun rev (ls)
  (foldl (flip cons) '() ls))

(defun not (x)
  (if x #f #t))

(def else #t)

(defmacro cond preds
  (if (nil? preds)
    '()
    (list 'if (caar preds)
          (cons 'begin (cdar preds))
          (cons 'cond (cdr preds)))))

(defmacro and values
  (cond
    [(nil? values) #t]
    [(nil? (cdr values)) (car values)]
    [else ((fun (tmp)
             (list (list 'fun (list tmp)
                         (list 'if tmp (cons 'and (cdr values)) tmp))
                   (car values)))
           (gensym))]))

(defmacro or values
  (cond
    [(nil? values) #f]
    [(nil? (cdr values)) (car values)]
    [else ((fun (tmp)
             (list (list 'fun (list tmp)
                         (list 'if tmp tmp (cons 'or (cdr values))))
                   (car values)))
           (gensym))]))

(defun all (f xs)
  (if (nil? xs)
    #t
    (and (f (car xs))
         (all f (cdr xs)))))

(defun any (f xs)
  (if (nil? xs)
    #f
    (or (f (car xs))
        (any f (cdr xs)))))

(defmacro quasiquote ls
  (*qq 0 (car ls)))

(defun *qq (rank x)
  (if (cons? x)
    (cond
      [(= (car x) 'unquote)
       (if (= rank 0)
         (cadr x)
         (list 'list (list 'quote 'unquote) (*qq (- rank 1) (cadr x))))]
      [(and (cons? (car x)) (= (caar x) 'unquote-splicing))
       (if (= rank 0)
         (list 'append (cadar x) (*qq rank (cdr x)))
         (list 'cons (list 'list (list 'quote 'unquote-splicing) (*qq (- rank 1) (cadar x))) (*qq rank (cdr x))))]
      [(= (car x) 'quasiquote)
       (list 'list (list 'quote 'quasiquote) (*qq (+ rank 1) (cadr x)))]
      [else
        (list 'cons (*qq rank (car x)) (*qq rank (cdr x)))])
    (list 'quote x)))

(defun *bind? (x)
  (and (cons? x)
       (cons? (cdr x))
       (nil? (cddr x))
       (sym? (car x))))

(defmacro let (binds . body)
  (cond
    [(sym? binds)
      `(named-let ,binds ,@body)]
    [(nil? binds)
      `(begin ,@body)]
    [(not (and (cons? binds) (*bind? (car binds))))
      (error "Syntax error: expected (let ((id expr)...) body...)")]
    [else
      `((fun (,(caar binds)) (let ,(cdr binds) ,@body))
        ,(cadar binds))]))

(defmacro letrec (binds . body)
  (if (and (list? binds) (all *bind? binds))
    (let ([vars (map (fun (x) `[,(car x) '()]) binds)]
          [inits (map (fun (x) `(set! ,(car x) ,(cadr x))) binds)])
      `(let ,vars ,@inits ,@body))
    (error "Syntax error: expected (letrec ((id expr)...) body...)")))

(defmacro named-let (sym binds . body)
  (if (and (list? binds) (all *bind? binds))
    (let ([args (map car binds)])
      `(let ,binds (letrec ([,sym (fun ,args ,@body)]) (,sym ,@args))))
    (error "Syntax error: expected (let sym ((id expr)...) body...)")))

(defmacro when (cond . body)
  `(if ,cond (begin ,@body) '()))

(defmacro unless (cond . body)
  `(if ,cond '() (begin ,@body)))

(defmacro let1 (var expr . body)
  `(let ([,var ,expr]) ,@body))

(defbuiltin call/cc (fun))

(defmacro let/cc (k . body)
  `(call/cc (fun (,k) ,@body)))

(defmacro shift (k . body)
  `(*shift (fun (,k) ,@body)))

(defmacro reset body
  `(*reset (fun () ,@body)))

(def *cont #f)

(defun *abort (thunk)
  (let1 v (thunk)
    (*cont v)))

(defun *reset (thunk)
  (let1 cont *cont
    (let/cc k
      (set! *cont (fun (v)
                    (set! *cont cont)
                    (k v)))
      (*abort thunk))))

(defun *shift (f)
  (let/cc k
    (*abort (fun ()
              (f (fun vs
                   (reset (apply k vs))))))))

(defbuiltin eval (s))
(defbuiltin macroexpand (s))
(defbuiltin macroexpand-1 (s))

;;;;;;;;;

(defbuiltin print strs)
(defbuiltin newline ())

(defun println strs
  (apply print strs)
  (newline))

(defbuiltin show (x))

(defun p xs
  (apply print (map show xs))
  (newline))

(defun count (xs)
  (let loop ([xs xs] [c 0])
    (if (nil? xs)
      c
      (loop (cdr xs) (+ c 1)))))

(defun nth (n xs)
  (if (= n 0)
    (car xs)
    (nth (- n 1) (cdr xs))))
