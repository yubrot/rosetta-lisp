; (ref (cons <0> (ref (cons <1> (ref (cons <2> (ref ...)))))))
(defun stream (input)
  (def head (ref ()))
  (defun forward ()
    (let1 next-head (ref ())
      (head (cons (input) next-head))
      (set! head next-head)))
  (cons head forward))

(defun stream-peek (s)
  (let ([p (car s)]
        [forward (cdr s)])
    (when (nil? (p)) (forward))
    (car (p))))

(defun stream-next (s)
  (if (= (stream-peek s) 'eof)
    s
    (let ([p (car s)]
          [forward (cdr s)])
      (cons (cdr (p)) forward))))

(defun stream-get (s)
  (if (stream-eof? s)
    (failure "eof")
    (let ([r (stream-peek s)]
          [s (stream-next s)])
      (success (cons r s)))))

(defun stream-take (n s)
  (result-reify
    (let loop ([n n] [s s])
      (if (= n 0)
        ()
        (let1 r (result-reflect (stream-get s))
          (cons (car r) (loop (- n 1) (cdr r))))))))

(defun stream-eof? (s)
  (= (stream-peek s) 'eof))

;! > (def _x 0)
;! ()
;! > (def _s1 (stream (fun () (set! _x (+ _x 1)) _x)))
;! ()
;! > (stream-peek _s1)
;! 1
;! > (stream-peek _s1)
;! 1
;! > (def _s2 (stream-next _s1))
;! ()
;! > (cons (stream-peek _s1) (stream-peek _s2))
;! (1 . 2)
;! > (set! _s1 (stream-next (stream-next _s1)))
;! ()
;! > (cons (stream-peek _s1) (stream-peek _s2))
;! (3 . 2)
;! > (set! _s1 (force-success (stream-get _s1)))
;! ()
;! > (cons (car _s1) (stream-peek (cdr _s1)))
;! (3 . 4)
;! > (force-success (stream-take 4 _s2))
;! (2 3 4 5)

(defun list->stream (ls)
  (stream
    (fun ()
      (if (nil? ls)
        'eof
        (let1 r (car ls)
          (set! ls (cdr ls))
          r)))))
;! > (force-success (stream-take 4 (list->stream (list 1 2 3 4 5))))
;! (1 2 3 4)

(defun stream->list (s)
  (if (stream-eof? s)
    ()
    (cons (stream-peek s) (stream->list (stream-next s)))))
;! > (stream->list (list->stream (list 1 2 3)))
;! (1 2 3)

(defun str->stream (str)
  (let1 i 0
    (stream
      (fun ()
        (if (< i (str-bytesize str))
          (let1 r (str-ref str i)
            (set! i (+ i 1))
            r)
          'eof)))))
;! > (force-success (stream-take 3 (str->stream "abc")))
;! (97 98 99)
;! > (force-success (stream-take 4 (str->stream "abc")))
;! fail

(defun stream->str (s)
  (apply str (stream->list s)))
;! > (stream->str (list->stream (list 97 98 99)))
;! "abc"

(defun port->stream (port)
  (stream
    (fun ()
      (force-success (read-byte port)))))
