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

(defun id (a)
  a)
;! > (id 0)
;! 0
;! > (id "foo")
;! "foo"

(defun compose (f g)
  (fun (x) (f (g x))))
;! > ((compose car cdr) (list 12 34 56))
;! 34

(defun flip (f)
  (fun (a b) (f b a)))
;! > ((flip (fun (a b) a)) 12 34)
;! 34

(defun const (v)
  (fun _ v))
;! > ((const 123))
;! 123
;! > ((const 123) 456 789)
;! 123

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
(defbuiltin port? (x))
(defbuiltin vec? (x))
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
;! > (list (port? 123) (vec? 123))
;! (#f #f)

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

(defun filter (f xs)
  (if (nil? xs)
    ()
    (if (f (car xs))
      (cons (car xs) (filter f (cdr xs)))
      (filter f (cdr xs)))))
;! > (filter num? (list 1 2 "foo" 3 'bar 4))
;! (1 2 3 4)

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

(defun partial (f . args-1)
  (fun args-2
    (apply f (append args-1 args-2))))
;! > ((partial +))
;! 0
;! > ((partial -) 1)
;! -1
;! > ((partial - 3) 1)
;! 2
;! > ((partial - 3 2) 1)
;! 0
;! > ((partial - 5 1) 2 3)
;! -1

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

(defun success (v) (cons #t v))
(defun failure (v) (cons #f v))

(defun result (v) (cdr v))

(defun success? (v) (car v))
(defun failure? (v) (not (car v)))

(defun force-success (v)
  (if (success? v)
    (result v)
    (error (result v))))

(defun force-failure (v)
  (if (failure? v)
    (result v)
    (error "force-failure")))

;! > (force-success (success 123))
;! 123
;! > (force-success (failure "error"))
;! fail
;! > (force-failure (success 123))
;! fail
;! > (force-failure (failure "error"))
;! "error"

(def result-unit success)

(defun result-bind (m f)
  (if (success? m) (f (result m)) m))

(defmacro result-reify body
  `(reset (result-unit (begin ,@body))))

(defun result-reflect (m)
  (shift k (result-bind m k)))

;! > (result-reify 123)
;! (#t . 123)
;! > (result-reify (+ (result-reflect (success 123)) 456))
;! (#t . 579)
;! > (result-reify (+ (result-reflect (failure "error")) 456))
;! (#f . "error")
;! > (result-reify (let1 a (result-reify 123) (+ (result-reflect a) 1)))
;! (#t . 124)
;! > (result-reify (let1 a (result-reify (result-reflect (success 123))) (+ (result-reflect a) 1)))
;! (#t . 124)
;! > (result-reify (let1 a (result-reify (result-reflect (failure "error"))) (+ (result-reflect a) 1)))
;! (#f . "error")
;! > (result-reify (let1 a (result-reify (result-reflect (failure "error"))) a))
;! (#t #f . "error")

(def list-concat append)

(defun list-count (xs)
  (let loop ([xs xs] [c 0])
    (if (nil? xs)
      c
      (loop (cdr xs) (+ c 1)))))
;! > (list-count (list 1 3 4 5 6))
;! 5

(defun list-find (f ls)
  (cond
    [(nil? ls) ()]
    [(f (car ls)) (car ls)]
    [else (list-find f (cdr ls))]))
;! > (list-find num? (list "foo" 'bar 123 "baz" 456))
;! 123
;! > (list-find num? (list "foo" 'bar "baz"))
;! ()

(defun list-lookup (k ls)
  (cond
    [(nil? ls) ()]
    [(= (caar ls) k) (cdar ls)]
    [else (list-lookup k (cdr ls))]))
;! > (list-lookup 2 '((1 . "foo") (2 . "bar") (3 . "baz")))
;! "bar"
;! > (list-lookup 5 '((1 . "foo") (2 . "bar") (3 . "baz")))
;! ()

(defun list-zip-with (f xs ys)
  (if (or (nil? xs) (nil? ys))
    ()
    (cons (f (car xs) (car ys))
          (list-zip-with f (cdr xs) (cdr ys)))))

(defun list-zip (xs ys)
  (list-zip-with cons xs ys))

;! > (list-zip '(1 2 3) '(a b c))
;! ((1 . a) (2 . b) (3 . c))
;! > (list-zip '(1 2 3) '(x y))
;! ((1 . x) (2 . y))

(def list-ref (flip nth))
;! > (list-ref (list 4 3 2) 0)
;! 4

(def list-at nth)

(defbuiltin str bytes)
;! > (str 102)
;! "f"
;! > (str 102 111 111 98 97 114)
;! "foobar"
;! > (str 260)
;! fail

(defbuiltin str-ref (str n))
;! > (str-ref "foobar" 0)
;! 102
;! > (str-ref "foobar" 1)
;! 111
;! > (str-ref "foobar" 8)
;! ()

(def str-at (flip str-ref))

(defbuiltin str-bytesize (str))
;! > (str-bytesize "foobar")
;! 6
;! > (str-bytesize "foobar" "baz")
;! fail
;! > (str-bytesize "日本語")
;! 9

(defun str->list (str)
  (map (partial str-ref str) (iota 0 (str-bytesize str))))
;! > (str->list "foobar")
;! (102 111 111 98 97 114)

(defun list->str (list)
  (apply str list))
;! > (list->str (list 102 111 111 98 97 114))
;! "foobar"

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
;! > (substr "foobar" 1 10)
;! fail

(defbuiltin sym->str (sym))
;! > (sym->str 'foo-bar)
;! "foo-bar"

(defbuiltin num->str (num))
;! > (num->str 123)
;! "123"

(defbuiltin str->num (num))
;! > (str->num "456")
;! 456
;! > (str->num "foo")
;! ()

(defun str-escape (str)
  (list->str (*bytes-escape (str->list str))))

(defun *bytes-escape (bytes)
  (if (nil? bytes)
    ()
    (let ([l (car bytes)]
          [r (*bytes-escape (cdr bytes))])
      (cond
        [(= l 92) (append '(92  92) r)] ; \\
        [(= l  9) (append '(92 116) r)] ; \t
        [(= l 10) (append '(92 110) r)] ; \n
        [(= l 34) (append '(92  34) r)] ; \"
        [else (cons l r)]))))

;! > (str-escape "foo")
;! "foo"
;! > (str-escape "foo\"bar")
;! "foo\\\"bar"
;! > (str-escape "\t\t\n")
;! "\\t\\t\\n"
;! > (str-escape "peo\\ple")
;! "peo\\\\ple"

(defun str-unescape (str)
  (list->str (*bytes-unescape (str->list str))))

(defun *bytes-unescape (bytes)
  (cond
    [(or (nil? bytes) (nil? (cdr bytes))) bytes]
    [(= (car bytes) 92)
      (let ([l (cadr bytes)]
            [r (*bytes-unescape (cddr bytes))])
        (cond
          [(= l  92) (cons 92 r)] ; \\
          [(= l 116) (cons  9 r)] ; \t
          [(= l 110) (cons 10 r)] ; \n
          [(= l 34)  (cons 34 r)] ; \"
          [else (cons l r)]))]
    [else (cons (car bytes) (*bytes-unescape (cdr bytes)))]))

;! > (str-unescape "foo")
;! "foo"
;! > (str-unescape "foo\\\"bar")
;! "foo\"bar"
;! > (str-unescape "\\t\\t\\n")
;! "\t\t\n"
;! > (str-unescape "peo\\\\ple")
;! "peo\\ple"

(defun str-trim (s)
  (str-trim-left (str-trim-right s)))

(defun str-trim-left (s)
  (let1 i (let loop ([i 0])
            (if (trim-target? (str-ref s i))
              (loop (+ i 1))
              i))
    (if (= i 0)
      s
      (substr s i (- (str-bytesize s) i)))))

(defun str-trim-right (s)
  (let1 i (let loop ([i (- (str-bytesize s) 1)])
            (if (trim-target? (str-ref s i))
              (loop (- i 1))
              i))
    (if (= i (- (str-bytesize s) 1))
      s
      (substr s 0 (+ i 1)))))

(defun trim-target? (c)
  (or (= 32 c) (= 10 c) (= 9 c)))

;! > (str-trim-left "foo")
;! "foo"
;! > (str-trim-left " foo")
;! "foo"
;! > (str-trim-left "   \n foo")
;! "foo"
;! > (str-trim-right "foo")
;! "foo"
;! > (str-trim-right "foo ")
;! "foo"
;! > (str-trim-right "foo   \n ")
;! "foo"

(defbuiltin vec items)
;! > (vec 1 2 3)
;! (vec 1 2 3)
;! > (vec? (vec 1 2 3))
;! #t

(defbuiltin vec-make (length init))
;! > (vec-make 5 #f)
;! (vec #f #f #f #f #f)

(defbuiltin vec-ref (vec n))
;! > (vec-ref (vec 4 9 3) 1)
;! 9

(def vec-at (flip vec-ref))

(defbuiltin vec-length (vec))
;! > (vec-length (vec))
;! 0
;! > (vec-length (vec 1 2 3 4 5))
;! 5

(defbuiltin vec-set! (vec n item))
;! > (let1 v (vec-make 3 #f)
;! >   (vec-set! v 0 #t)
;! >   (vec-set! v 2 "k")
;! >   v)
;! (vec #t #f "k")

(defbuiltin vec-copy! (dest dest-start src src-start length))
;! > (let ([fs (vec-make 5 #f)]
;! >       [ts (vec-make 3 #t)])
;! >   (vec-set! ts 1 "k")
;! >   (vec-copy! fs 1 ts 0 2)
;! >   fs)
;! (vec #f #t "k" #f #f)

(defun vec->list (vec)
  (map (partial vec-ref vec) (iota 0 (vec-length vec))))
;! > (vec->list (vec 1 3 5 7))
;! (1 3 5 7)

(defun list->vec (list)
  (apply vec list))
;! > (list->vec (list 1 3 5 7))
;! (vec 1 3 5 7)

(defmacro defrecord (name constructor-name predicate-name fields)
  (unless (and (sym? name)
               (or (sym? constructor-name) (= #f constructor-name))
               (or (sym? predicate-name) (= #f predicate-name))
               (list? fields)
               (all list? fields)
               (all (compose not nil?) fields)
               (all (partial all sym?) fields))
    (error "Syntax error: expected (defrecord name predicate-name (fields...))"))
  (let ([field-names (map car fields)]
        [field-getter-names (map (fun (field) (and (cons? (cdr field)) (cadr field))) fields)]
        [field-setter-names (map (fun (field) (and (cons? (cdr field)) (cons? (cddr field)) (caddr field))) fields)]
        [field-indices (iota 1 (+ 1 (list-count fields)))]
        [constructor (and constructor-name `(defun ,constructor-name ,field-names (vec ',name ,@field-names)))]
        [predicate (and predicate-name `(defun ,predicate-name (v) (and (vec? v) (= ',name (vec-ref v 0)))))]
        [getters (list-zip-with (fun (i f) (and f `(defun ,f (v) (vec-ref v ,i)))) field-indices field-getter-names)]
        [setters (list-zip-with (fun (i f) (and f `(defun ,f (v x) (vec-set! v ,i x)))) field-indices field-setter-names)])
    `(begin
       ,constructor
       ,predicate
       ,@(filter id getters)
       ,@(filter id setters))))

;! > (defrecord point point point?
;! >   ([x point-x]
;! >    [y point-y set-point-y!]))
;! ()
;! > (def _p (point 12 34))
;! ()
;! > (list (point? _p) (point? 123))
;! (#t #f)
;! > (list (point-x _p) (point-y _p))
;! (12 34)
;! > (set-point-y! _p 56)
;! ()
;! > (list (point-x _p) (point-y _p))
;! (12 56)

(defbuiltin open (filepath mode))
(defbuiltin close (port))

(def stdin ((builtin stdin)))
(def stdout ((builtin stdout)))
(def stderr ((builtin stderr)))

(defbuiltin read-byte (port))
(defbuiltin read-str (size port))
(defbuiltin read-line (port))

(defun read-all (port)
  (result-reify
    (let loop ([buf ""])
      (let ([str-read (result-reflect (read-str 4096 port))])
        (if (= 'eof str-read)
          buf
          (loop (str-concat buf str-read)))))))

(defun open-read (filepath)
  (result-reify
    (let ([port (result-reflect (open filepath "r"))]
          [r (result-reflect (read-all port))])
      (result-reflect (close port))
      r)))

(defun get-byte () (force-success (read-byte stdin)))
(defun get-line () (force-success (read-line stdin)))
(defun get-all () (force-success (read-all stdin)))

(defbuiltin write-byte (byte port))
(defbuiltin write-str (str port))
(defbuiltin write-line (str port))

(defun write-all (str port)
  (result-reify
    (let loop ([buf str])
      (let ([bytesize-wrote (result-reflect (write-str buf port))]
            [bytesize-rest (- (str-bytesize buf) bytesize-wrote)])
        (if (= 0 bytesize-rest)
          ()
          (loop (substr buf bytesize-wrote bytesize-rest)))))))

(defun write-newline (port)
  (write-line "" port))

(defun open-write (filepath str)
  (result-reify
    (let ([port (result-reflect (open filepath "w"))]
          [r (result-reflect (write-all str port))])
      (result-reflect (close port))
      r)))

(defun put-byte (byte) (force-success (write-byte byte stdout)))
(defun put-line (str) (force-success (write-line str stdout)))
(defun put-all (str) (force-success (write-all str stdout)))
(defun put-newline () (force-success (write-newline stdout)))

(defun inspect (x)
  (cond
    [(num? x) (num->str x)]
    [(sym? x) (sym->str x)]
    [(str? x) (str-concat "\"" (str-escape x) "\"")]
    [(cons? x) (let ([l (car x)]
                     [r (cdr x)]
                     [a (list-lookup l *syntax-sugar)])
                 (if (and (not (nil? a)) (cons? r) (nil? (cdr r)))
                   (str-concat a (inspect (car r)))
                   (str-concat "(" (*inspect-cons l r) ")")))]
    [(nil? x) "()"]
    [(= #t x) "#t"]
    [(= #f x) "#f"]
    [(proc? x) "<proc>"]
    [(meta? x) "<meta>"]
    [(port? x) "<port>"]
    [(vec? x) (inspect (cons 'vec (vec->list x)))]
    [else (error)]))

(def *syntax-sugar
  '((quote . "'")
    (quasiquote . "`")
    (unquote . ",")
    (unquote-splicing . ",@")))

(defun *inspect-cons (a b)
  (cond
    [(nil? b) (inspect a)]
    [(cons? b) (str-concat (inspect a) " " (*inspect-cons (car b) (cdr b)))]
    [else (str-concat (inspect a) " . " (inspect b))]))

;! > (inspect 123)
;! "123"
;! > (inspect 'foo)
;! "foo"
;! > (inspect "Hello, World!\n")
;! "\"Hello, World!\\n\""
;! > (inspect ())
;! "()"
;! > (inspect '(1))
;! "(1)"
;! > (inspect '(1 a "b"))
;! "(1 a \"b\")"
;! > (inspect '(foo . bar))
;! "(foo . bar)"
;! > (inspect '(foo bar . baz))
;! "(foo bar . baz)"
;! > (map inspect (list ''foo ''(bar baz)))
;! ("'foo" "'(bar baz)")
;! > (inspect '`(foo ,bar ,@baz))
;! "`(foo ,bar ,@baz)"
;! > (inspect '(quote foo bar))
;! "(quote foo bar)"
;! > (inspect '(quote . foo))
;! "(quote . foo)"
;! > (map inspect '(#t #f))
;! ("#t" "#f")
;! > (map inspect (list (fun ()) = (macro ()) def))
;! ("<proc>" "<proc>" "<meta>" "<meta>")
;! > (map inspect (list stdin stdout stderr))
;! ("<port>" "<port>" "<port>")
;! > (inspect (vec 1 2 3))
;! "(vec 1 2 3)"

(defun print strs
  (map put-all strs)
  ())

(defun println strs
  (map put-all strs)
  (put-newline)
  ())

(defun p xs
  (apply println (map inspect xs)))

(def args ((builtin args)))

(defbuiltin eval (s))
;! > (force-success (eval '(+ 1 2 3)))
;! 6
;! > (force-success (eval '(error)))
;! fail

(defbuiltin macroexpand (s))
(defbuiltin macroexpand-1 (s))
;! > (force-success (macroexpand 123))
;! 123
;! > (force-success (macroexpand '(defun foo (x y) (+ x y))))
;! (def foo (fun (x y) (+ x y)))
;! > (def _skip (macro (a . b) b))
;! ()
;! > (force-success (macroexpand '(_skip 12 _skip 34 list 56 78)))
;! (list 56 78)
;! > (force-success (macroexpand-1 '(_skip 12 _skip 34 list 56 78)))
;! (_skip 34 list 56 78)
;! > (force-success (macroexpand '(list 12 (_skip 34 list 56 78))))
;! (list 12 (list 56 78))
;! > (force-success (macroexpand-1 '(list 12 (_skip 34 list 56 78))))
;! (list 12 (_skip 34 list 56 78))
;! > (force-success (macroexpand '(_skip)))
;! fail
