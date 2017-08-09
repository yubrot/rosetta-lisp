(defun *str-hash (str)
  (let loop ([i 0]
             [r 0]
             [p 1])
    (if (<= (str-bytesize str) i)
      r
      (loop (+ i 1)
            (+ r (* p (str-ref str i)))
            (% (* p 257) 2038177)))))

(def *tbl-capacity-provider
  (list->stream (list 17 31 61 101 211 421 877 1663 3323 6871 14173 28439 57457 112771 232607)))

(def *tbl-threshold 0.8)

(defun *tbl-f (hash M)
  (% hash M))

(defun *tbl-g (hash M)
  (+ 1 (% hash (- M 1))))

(defun *tbl-h (hash M i)
  (% (+ (*tbl-f hash M)
        (* i (*tbl-g hash M)))
     M))

; <tbl> = (vec 'tbl capacity-stream length removed <payload>)
; <payload> = (vec <item> ...)
; <item> = (key . value) | key | #f

(defun tbl ()
  (let1 t (vec-make 5 ())
    (vec-set! t 0 'tbl)
    (tbl-clear! t)
    t))

(defun tbl-clear! (t)
  (vec-set! t 1 *tbl-capacity-provider)
  (vec-set! t 2 0)
  (vec-set! t 3 0)
  (vec-set! t 4 (vec-make (stream-peek *tbl-capacity-provider) #f)))

(defun tbl? (x)
  (and (vec? x)
       (= (vec-ref x 0) 'tbl)))

(defun *tbl-capacity (tbl) (vec-ref tbl 1))
(defun *tbl-length-total (tbl) (vec-ref tbl 2))
(defun *tbl-length-removed (tbl) (vec-ref tbl 3))
(defun *tbl-payload (tbl) (vec-ref tbl 4))

(defun tbl-justify! (tbl additional-capacity)
  (let ([payload-required-length (+ (*tbl-length-total tbl) additional-capacity)]
        [required-length (- payload-required-length (*tbl-length-removed tbl))]
        [usable-length (* (stream-peek (*tbl-capacity tbl)) *tbl-threshold)])
    ; NOTE: tbl never shrinks.
    (when (< usable-length payload-required-length)
      (let ([payload (*tbl-payload tbl)]
            [new-capacity (*tbl-forward-capacity (*tbl-capacity tbl) required-length)]
            [new-payload (vec-make (stream-peek new-capacity) #f)])
        (vec-set! tbl 1 new-capacity)
        (vec-set! tbl 2 0)
        (vec-set! tbl 3 0)
        (vec-set! tbl 4 new-payload)
        (*tbl-migrate! payload tbl)))))

(defun *tbl-forward-capacity (s length)
  (cond
    [(= 'eof (stream-peek s)) (error "Too much elements")]
    [(< (* (stream-peek s) *tbl-threshold) length) (*tbl-forward-capacity (stream-next s) length)]
    [else s]))

(defun *tbl-migrate! (payload tbl)
  (let loop ([i 0])
    (when (< i (vec-length payload))
      (let1 item (vec-ref payload i)
        (when (cons? item)
          (tbl-insert! tbl (car item) (cdr item)))
        (loop (+ i 1))))))

(defun *tbl-find-index (tbl key)
  (let ([M (stream-peek (*tbl-capacity tbl))]
        [hash (*str-hash key)]
        [match? (fun (v)
                  (or (not v)
                      (= v key)
                      (and (cons? v) (= (car v) key))))])
    (let loop ([i 0])
      (let1 index (*tbl-h hash M i)
        (if (match? (vec-ref (*tbl-payload tbl) index))
          index
          (loop (+ i 1)))))))

(defun *tbl-find (tbl key)
  (vec-ref (*tbl-payload tbl) (*tbl-find-index tbl key)))

(defun tbl-contains? (tbl key)
  (cons? (*tbl-find tbl key)))

(defun tbl-find (tbl key)
  (let1 r (*tbl-find tbl key)
    (if (cons? r) (cdr r) ())))

(defun tbl-insert! (tbl key value)
  (tbl-justify! tbl 1)
  (let ([index (*tbl-find-index tbl key)]
        [prev (vec-ref (*tbl-payload tbl) index)])
    (vec-set! (*tbl-payload tbl) index (cons key value))
    (cond
      [(not prev) (vec-set! tbl 2 (+ (*tbl-length-total tbl) 1))]
      [(str? prev) (vec-set! tbl 3 (- (*tbl-length-removed tbl) 1))])))

(defun tbl-remove! (tbl key)
  (let ([index (*tbl-find-index tbl key)]
        [prev (vec-ref (*tbl-payload tbl) index)])
    (when (cons? prev)
      (vec-set! (*tbl-payload tbl) index key)
      (vec-set! tbl 3 (+ (*tbl-length-removed tbl) 1)))))

;! > (tbl? (tbl))
;! #t
;! > (tbl? 123)
;! #f
;! > (def _t (tbl))
;! ()
;! > (tbl-contains? _t "foo")
;! #f
;! > (tbl-insert! _t "foo" 123)
;! ()
;! > (tbl-contains? _t "foo")
;! #t
;! > (tbl-find _t "foo")
;! 123
;! > (list (tbl-contains? _t "bar") (tbl-find _t "bar"))
;! (#f ())
;! > (tbl-insert! _t "bar" 456)
;! ()
;! > (list (tbl-find _t "foo") (tbl-find _t "bar") (tbl-find _t "baz"))
;! (123 456 ())
;! > (tbl-remove! _t "foo")
;! ()
;! > (tbl-remove! _t "baz")
;! ()
;! > (list (tbl-find _t "foo") (tbl-find _t "bar") (tbl-find _t "baz"))
;! (() 456 ())
;! > (begin (for (iota 0 1000) (fun (i) (tbl-insert! _t (num->str i) (* i 2)))) ())
;! ()
;! > (foldr + 0 (map (fun (i) (tbl-find _t (num->str i))) (iota 0 301)))
;! 90300

(defun *tbl-information (tbl)
  (list 'tbl-information
        (list 'capacity (stream-peek (*tbl-capacity tbl)))
        (list 'used (*tbl-length-total tbl))
        (list 'removed (*tbl-length-removed tbl))
        (list 'items (filter cons? (vec->list (*tbl-payload tbl))))))
