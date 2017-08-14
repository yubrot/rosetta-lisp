(defun parse-just (p stream)
  (let1 p (p-reduce (fun (x _) x) p ps-eof)
    (result-reify
      (car (result-reflect (parse p stream))))))

;! > (defun ps-test (p str)
;! >   (let1 r (p (str->stream str))
;! >     (and r (cons (car r) (stream->str (cdr r))))))
;! ()

; Predicates

(defun char-class (s)
  (let ([inverse? (= (str-ref s 0) 94)]
        [ls (str->list s)]
        [fs (*char-class (if inverse? (cdr ls) ls))])
    (if inverse?
      (fun (i) (all (fun (f) (not (f i))) fs))
      (fun (i) (any (fun (f) (f i)) fs)))))

(defun *char-class (ls)
  (cond
    [(nil? ls) ()]
    [(or (nil? (cdr ls)) (nil? (cddr ls))) (map *char-class-unit ls)]
    [(= (nth 1 ls) 45) (cons (*char-class-range (nth 0 ls) (nth 2 ls))
                             (*char-class (cdddr ls)))]
    [else (cons (*char-class-unit (car ls))
                (*char-class (cdr ls)))]))

(defun *char-class-unit (x)
  (fun (i) (= i x)))

(defun *char-class-range (x y)
  (fun (i) (<= x i y)))

;! > (map (char-class "a-fstx-z") (str->list "abcfgtuwy"))
;! (#t #t #t #t #f #t #f #f #t)
;! > (map (char-class "^a-fstx-z") (str->list "abcfgtuwy"))
;! (#f #f #f #f #t #f #t #t #f)

; Parser combinators

(defun ps-any (i)
  (and (not (= (stream-peek i) 'eof)) (cons (stream-peek i) (stream-next i))))
;! > (ps-test ps-any "abc")
;! (97 . "bc")
;! > (ps-test ps-any "")
;! #f

(defun ps-eof (i)
  (and (= (stream-peek i) 'eof) (cons () i)))
;! > (ps-test ps-eof "")
;! (() . "")
;! > (ps-test ps-eof "abc")
;! #f

; p-unit
;! > (ps-test (p-unit "Hello") "abc")
;! ("Hello" . "abc")

; p-bind
;! > (ps-test (p-bind ps-any (fun (a)
;! >            (p-bind ps-any (fun (b)
;! >              (p-unit (cons a b)))))) "abc")
;! ((97 . 98) . "c")

; p-fail
;! > (ps-test p-fail "abc")
;! #f
;! > (ps-test (p-bind ps-any (fun (a)
;! >            (p-bind p-fail (fun (b)
;! >              (p-unit (cons a b)))))) "abc")
;! #f

(def ps-char
  (p-map str ps-any))
; p-map
;! > (ps-test ps-char "abc")
;! ("a" . "bc")

(defun ps-if (f)
  (p-where f ps-any))
; p-where
;! > (ps-test (ps-if (char-class "ab")) "abc")
;! (97 . "bc")
;! > (ps-test (ps-if (char-class "ab")) "def")
;! #f

(defun ps-char-if (f)
  (p-map str (ps-if f)))
;! > (ps-test (ps-char-if (char-class "ab")) "abc")
;! ("a" . "bc")

; p-or, p-choice
;! > (let ([ab (ps-char-if (char-class "ab"))]
;! >       [ac (ps-char-if (char-class "ac"))]
;! >       [add-suffix (fun (s) (str-concat s "-"))]
;! >       [p (p-choice ab (p-map add-suffix ac))])
;! >   (list (ps-test p "abcd")
;! >         (ps-test p "bcda")
;! >         (ps-test p "cdab")
;! >         (ps-test p "dabc")))
;! (("a" . "bcd") ("b" . "cda") ("c-" . "dab") #f)

; p-cons, p-nil, p-seq
;! > (let1 p (p-seq (ps-char-if (char-class "ab"))
;! >                (ps-char-if (char-class "12"))
;! >                (ps-char-if (char-class "xy")))
;! >   (list (ps-test p "c2xo")
;! >         (ps-test p "a3yp")
;! >         (ps-test p "b1zq")
;! >         (ps-test p "a1xr")))
;! (#f #f #f (("a" "1" "x") . "r"))

(defun ps-list (xs)
  (apply p-seq (map (fun (x) (ps-if (fun (y) (= x y)))) xs)))

(defun ps-str (s)
  (p-map (const s) (ps-list (str->list s))))

;! > (ps-test (ps-str "foo") "bar")
;! #f
;! > (ps-test (ps-str "bar") "bar")
;! ("bar" . "")
;! > (ps-test (ps-str "baz") "bar")
;! #f

; p-many, p-some
;! > (let ([p0 (p-many (ps-str "a"))]
;! >       [p1 (p-some (ps-str "a"))])
;! >   (list (ps-test p0 "")
;! >         (ps-test p0 "a")
;! >         (ps-test p0 "aaa")
;! >         (ps-test p0 "aabb")
;! >         (ps-test p1 "")
;! >         (ps-test p1 "a")
;! >         (ps-test p1 "aabb")))
;! ((() . "") (("a") . "") (("a" "a" "a") . "") (("a" "a") . "bb") #f (("a") . "") (("a" "a") . "bb"))

; p-reduce
;! > (ps-test (p-reduce + (p-map (fun (n) (* n 10000)) ps-any)
;! >                      (p-map (fun (n) (* n 100)) ps-any)
;! >                      ps-any) "abcdef")
;! (979899 . "def")

(defun ps-str-while (f)
  (p-map (fun (ls) (apply str ls)) (p-some (ps-if f))))
;! > (ps-test (ps-str-while (char-class "abc")) "ababcbadcba")
;! ("ababcba" . "dcba")
;! > (ps-test (ps-str-while (char-class "abc")) "defabcdef")
;! #f
