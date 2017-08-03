(def cons (builtin cons))
;! > (cons 1)
;! fail
;! > (cons 1 2)
;! (1 . 2)
;! > (cons 1 2 3)
;! fail

(def list (fun xs xs))

(def defun (macro (sym . body) (list 'def sym (cons 'fun body))))
(def defmacro (macro (sym . body) (list 'def sym (cons 'macro body))))
(def defbuiltin (macro (sym . intf) (list 'def sym (list 'builtin sym))))
;! > (defun _f (a b) b)
;! ()
;! > (_f 3 5)
;! 5
;! > (defmacro _m (a . b) b)
;! ()
;! > (_m 1 _f 2 3)
;! 3

(defbuiltin exit (exitcode))
(defbuiltin error (msg))

(defbuiltin gensym ())

(defbuiltin car (cons))
(defbuiltin cdr (cons))
;! > (car (cons 12 34))
;! 12
;! > (cdr (cons 12 34))
;! 34

(defbuiltin apply (f args))
;! > (apply cons (list 12 34))
;! (12 . 34)

(defun compose (f g)
  (fun (x) (f (g x))))
;! > ((compose car cdr) (list 12 34 56))
;! 34

(defun flip (f)
  (fun (a b) (f b a)))
;! > ((flip (fun (a b) a)) 12 34)
;! 34

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
;! > (num? 123)
;! #t
;! > (num? 12 34)
;! fail
;! > (num? "foo")
;! #f
;! > (sym? 'foo)
;! #t
;! > (str? "foo")
;! #t
;! > (cons? (list 1 2 3))
;! #t
;! > (nil? ())
;! #t
;! > (list (bool? #t) (bool? ()))
;! (#t #f)
;! > (list (proc? (fun ())) (proc? cons) (proc? (macro ())) (proc? def))
;! (#t #t #f #f)
;! > (list (meta? (fun ())) (meta? cons) (meta? (macro ())) (meta? def))
;! (#f #f #t #t)

(defun list? (x)
  (if (nil? x)
    #t
    (if (cons? x)
      (list? (cdr x))
      #f)))
;! > (list? ())
;! #t
;! > (list? '(12 . 34))
;! #f
;! > (list? '(12 34 . 56))
;! #f
;! > (list? '(12 34 56))
;! #t

(defbuiltin + nums)
(defbuiltin - (num . nums))
(defbuiltin * nums)
(defbuiltin / (num . nums))
(defbuiltin % (num . nums))
;! > (list (+) (+ 11) (+ 3 4 5))
;! (0 11 12)
;! > (list (- 5) (- 5 2))
;! (-5 3)
;! > (list (*) (* 11) (* 3 4 5))
;! (1 11 60)
;! > (list (/ 2) (/ 20 5 2))
;! (0.5 2)
;! > (list (% 5) (% 5 3))
;! (5 2)

(defbuiltin = xs)
;! > (list (=) (= 1) (= 1 1) (= 1 2) (= 1 1 1) (= 1 1 2))
;! (#t #t #t #f #t #f)
;! > (list (= "foo" "foo") (= "foo" "bar"))
;! (#t #f)
;! > (list (= #t #t) (= #f #f) (= #t #f))
;! (#t #t #f)
;! > (list (= () ()) (= '(1 2) '(1 2)) (= '(1 2) '(1 3)) (= '(1 2 . 3) '(1 2 . 3)) (= '(1 2 3) '(1 2 . 3)))
;! (#t #t #f #t #f)
;! > (= (fun ()) (fun ()))
;! #f
;! > (list (= 123 "123") (= "foo" 'foo))
;! (#f #f)

(defbuiltin < nums-or-strs)
(defbuiltin > nums-or-strs)
(defbuiltin <= nums-or-strs)
(defbuiltin >= nums-or-strs)
;! > (list (<) (< 1) (< 1 2) (< 1 2 3) (< 1 3 3) (< 1 4 3) (< 4 3) (< 4 4 3))
;! (#t #t #t #t #f #f #f #f)
;! > (list (>) (> 1) (> 1 2) (> 1 2 3) (> 1 3 3) (> 1 4 3) (> 4 3) (> 4 4 3))
;! (#t #t #f #f #f #f #t #f)
;! > (list (<=) (<= 1) (<= 1 2) (<= 1 2 3) (<= 1 3 3) (<= 1 4 3) (<= 4 3) (<= 4 4 3))
;! (#t #t #t #t #t #f #f #f)
;! > (list (>=) (>= 1) (>= 1 2) (>= 1 2 3) (>= 1 3 3) (>= 1 4 3) (>= 4 3) (>= 4 4 3))
;! (#t #t #f #f #f #f #t #t)
;! > (list (< "abc" "cab") (< "abc" "abd") (< "bac" "acb"))
;! (#t #t #f)
;! > (< 123 "456")
;! fail
;! > (< #f)
;! fail

(defun map (f xs)
  (if (nil? xs)
    ()
    (cons (f (car xs)) (map f (cdr xs)))))
;! > (map (fun (a) (* a 3)) (list 1 2 5 4))
;! (3 6 15 12)

(def for (flip map))
;! > (for (list 1 2 3) (fun (a) (* a a)))
;! (1 4 9)

(defun foldl (f i xs)
  (if (nil? xs)
    i
    (foldl f (f i (car xs)) (cdr xs))))
;! > (foldl cons () (list 2 5 3))
;! (((() . 2) . 5) . 3)

(defun foldr (f i xs)
  (if (nil? xs)
    i
    (f (car xs) (foldr f i (cdr xs)))))
;! > (foldr cons () (list 2 5 3))
;! (2 5 3)

(defun append ls
  (foldr *append () ls))

(defun *append (a b)
  (if (nil? a)
    b
    (cons (car a) (*append (cdr a) b))))
;! > (append (list 1 2 3) (list 4 5 6) (list 7 8))
;! (1 2 3 4 5 6 7 8)

(defun reverse (ls)
  (foldl (flip cons) () ls))
;! > (reverse (list 1 2 4 5))
;! (5 4 2 1)

(defun nth (n xs)
  (if (= n 0)
    (car xs)
    (nth (- n 1) (cdr xs))))
;! > (nth 3 (list 9 8 7 6 5))
;! 6

(defun iota (a b)
  (if (< a b)
    (cons a (iota (+ a 1) b))
    '()))
;! > (iota 0 5)
;! (0 1 2 3 4)
;! > (iota 2 4)
;! (2 3)
;! > (iota 3 3)
;! ()

(defun not (x)
  (if x #f #t))
;! > (map not (list 123 () #t #f))
;! (#f #f #f #t)

(def else #t)

(defmacro cond preds
  (if (nil? preds)
    ()
    (list 'if (caar preds)
          (cons 'begin (cdar preds))
          (cons 'cond (cdr preds)))))
;! > (cond)
;! ()
;! > (cond [#t 123])
;! 123
;! > (cond [#t 123 456])
;! 456
;! > (cond [#t 1] [#t 2] [#t 3])
;! 1
;! > (cond [#f 1] [#t 2] [#t 3])
;! 2
;! > (cond [#f 1] [#f 2] [#t 3])
;! 3
;! > (cond [#f 1] [#f 2] [#f 3])
;! ()
;! > (def _r ())
;! ()
;! > (cond
;! >   [(begin (set! _r (cons 1 _r)) #f) (set! _r (cons 2 _r))]
;! >   [(begin (set! _r (cons 3 _r)) #t) (set! _r (cons 4 _r))]
;! >   [(begin (set! _r (cons 5 _r)) #t) (set! _r (cons 6 _r))])
;! ()
;! > _r
;! (4 3 1)

(defmacro and values
  (cond
    [(nil? values) #t]
    [(nil? (cdr values)) (car values)]
    [else ((fun (tmp)
             (list (list 'fun (list tmp)
                         (list 'if tmp (cons 'and (cdr values)) tmp))
                   (car values)))
           (gensym))]))
;! > (and)
;! #t
;! > (and 123)
;! 123
;! > (and 123 456)
;! 456
;! > (and #f 456)
;! #f
;! > (and 123 456 789)
;! 789
;! > (def _r ())
;! ()
;! > (and
;! >   (begin (set! _r (cons 1 _r)) 123)
;! >   (begin (set! _r (cons 2 _r)) 456)
;! >   (begin (set! _r (cons 3 _r)) #f)
;! >   (begin (set! _r (cons 4 _r)) 789))
;! #f
;! > _r
;! (3 2 1)

(defmacro or values
  (cond
    [(nil? values) #f]
    [(nil? (cdr values)) (car values)]
    [else ((fun (tmp)
             (list (list 'fun (list tmp)
                         (list 'if tmp tmp (cons 'or (cdr values))))
                   (car values)))
           (gensym))]))
;! > (or)
;! #f
;! > (or 123)
;! 123
;! > (or 123 456)
;! 123
;! > (or #f 456)
;! 456
;! > (or 123 456 789)
;! 123
;! > (def _r ())
;! ()
;! > (or
;! >   (begin (set! _r (cons 1 _r)) #f)
;! >   (begin (set! _r (cons 2 _r)) #f)
;! >   (begin (set! _r (cons 3 _r)) 123)
;! >   (begin (set! _r (cons 4 _r)) 456))
;! 123
;! > _r
;! (3 2 1)

(defun all (f xs)
  (if (nil? xs)
    #t
    (and (f (car xs))
         (all f (cdr xs)))))
;! > (all num? (list))
;! #t
;! > (all num? (list 1 2 3))
;! #t
;! > (all num? (list 1 "2" 3))
;! #f

(defun any (f xs)
  (if (nil? xs)
    #f
    (or (f (car xs))
        (any f (cdr xs)))))
;! > (any num? (list))
;! #f
;! > (any num? (list 1 2 3))
;! #t
;! > (any num? (list "1" 2 "3"))
;! #t
;! > (any num? (list "1" "2" "3"))
;! #f

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
      (error "Syntax error: expected (let ((name expr)...) body...)")]
    [else
      `((fun (,(caar binds)) (let ,(cdr binds) ,@body))
        ,(cadar binds))]))

(defmacro letrec (binds . body)
  (if (and (list? binds) (all *bind? binds))
    (let ([vars (map (fun (x) `[,(car x) ()]) binds)]
          [inits (map (fun (x) `(set! ,(car x) ,(cadr x))) binds)])
      `(let ,vars ,@inits ,@body))
    (error "Syntax error: expected (letrec ((name expr)...) body...)")))

(defmacro named-let (sym binds . body)
  (if (and (list? binds) (all *bind? binds))
    (let ([args (map car binds)])
      `(let ,binds (letrec ([,sym (fun ,args ,@body)]) (,sym ,@args))))
    (error "Syntax error: expected (named-let name ((name expr)...) body...)")))

;! > (let ([_x 2] [_y 3]) (* _x _y))
;! 6
;! > _x
;! fail
;! > (let _loop ([x 10] [sum 0])
;! >   (if (< 0 x)
;! >     (_loop (- x 1) (+ sum x))
;! >     sum))
;! 55
;! > _loop
;! fail
;! > (let ([x 3] [x (* x 4)] [x (+ x 5)]) x)
;! 17
;! > (letrec ([even? (fun (x) (if (= (% x 2) 0) #t (odd? (- x 1))))]
;! >          [odd? (fun (x) (if (= (% x 2) 0) #f (even? (- x 1))))])
;! >   (list (even? 4) (even? 5) (odd? 6) (odd? 7)))
;! (#t #f #f #t)

(defmacro when (cond . body)
  `(if ,cond (begin ,@body) ()))
;! > (when #f 123 456)
;! ()
;! > (when #t 123 456)
;! 456

(defmacro unless (cond . body)
  `(if ,cond () (begin ,@body)))
;! > (unless #f 123 456)
;! 456
;! > (unless #t 123 456)
;! ()

(defmacro let1 (var expr . body)
  `(let ([,var ,expr]) ,@body))
;! > (let1 x 3
;! >   (let1 x (* x 4)
;! >     (let1 x (+ x 5)
;! >       x)))
;! 17

(defbuiltin call/cc (fun))

(defmacro let/cc (k . body)
  `(call/cc (fun (,k) ,@body)))

;! > (+ 1 (let/cc cont (+ 10 (cont 100))))
;! 101
;! > (+ 1 (let/cc cont (+ 10 100)))
;! 111
;! > (let ([x 10] [sum 0] [cont #f])
;! >   (let/cc k (set! cont k))
;! >   (when (< 0 x)
;! >     (set! sum (+ sum x))
;! >     (set! x (- x 1))
;! >     (cont))
;! >   sum)
;! 55

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

;! > (reset
;! >   (shift k (append '(1) (k)))
;! >   (shift k (append '(2) (k)))
;! >   (shift k (append '(3) (k)))
;! >   '())
;! (1 2 3)

(defbuiltin eval (s))
;! > (eval '(+ 1 2 3))
;! (#t . 6)
;! > (car (eval '(error)))
;! #f

(defbuiltin macroexpand (s))
(defbuiltin macroexpand-1 (s))
;! > (macroexpand 123)
;! (#t . 123)
;! > (cdr (macroexpand '(defun foo (x y) (+ x y))))
;! (def foo (fun (x y) (+ x y)))
;! > (def _skip (macro (a . b) b))
;! ()
;! > (cdr (macroexpand '(_skip 12 _skip 34 list 56 78)))
;! (list 56 78)
;! > (cdr (macroexpand-1 '(_skip 12 _skip 34 list 56 78)))
;! (_skip 34 list 56 78)
;! > (cdr (macroexpand '(list 12 (_skip 34 list 56 78))))
;! (list 12 (list 56 78))
;! > (cdr (macroexpand-1 '(list 12 (_skip 34 list 56 78))))
;! (list 12 (_skip 34 list 56 78))
;! > (car (macroexpand '(_skip)))
;! #f

(def list-concat append)

(defun list-count (xs)
  (let loop ([xs xs] [c 0])
    (if (nil? xs)
      c
      (loop (cdr xs) (+ c 1)))))
;! > (list-count (list 1 3 4 5 6))
;! 5

(def list-ref (flip nth))
;! > (list-ref (list 4 3 2) 0)
;! 4

(defbuiltin str bytes)
;! > (str 102)
;! "f"
;! > (str 102 111 111 98 97 114)
;! "foobar"

(defbuiltin str-ref (str n))
;! > (str-ref "foobar" 0)
;! 102
;! > (str-ref "foobar" 1)
;! 111

(defbuiltin str-bytesize (str))
;! > (str-bytesize "foobar")
;! 6
;! > (str-bytesize "foobar" "baz")
;! fail
;! > (str-bytesize "日本語")
;! 9

(defun str->list (str)
  (map (fun (n) (str-ref str n)) (iota 0 (str-bytesize str))))
;! > (str->list "foobar")
;! (102 111 111 98 97 114)

(defun list->str (list)
  (apply str list))

(defbuiltin str-concat strs)
;! > (str-concat)
;! ""
;! > (str-concat "foo" "bar" "baz")
;! "foobarbaz"

(defbuiltin substr (str n bytesize))
;! > (substr "foobar" 0 3)
;! "foo"
;! > (substr "foobar" 2 3)
;! "oba"
;! > (substr "foobar" 1 4)
;! "ooba"
;! > (str->list (substr "日本語" 0 3))
;! (230 151 165)

(defbuiltin num->str (num))
;! > (num->str 123)
;! "123"

(defbuiltin str->num (num))
;! > (str->num "456")
;! 456

;;;;;;;;;;;;;

(defbuiltin print strs)
(defbuiltin newline ())

(defun println strs
  (apply print strs)
  (newline))

(defbuiltin inspect (x))

(defun p xs
  (apply print (map inspect xs))
  (newline))

