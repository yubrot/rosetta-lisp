(def fail (fun () (error "Cannot backtrack")))

(defun amb-proc l
  (let1 former-fail fail
    (if (nil? l)
      (fail)
      (call/cc (fun (k)
        (set! fail (fun ()
          (set! fail former-fail)
          (k (apply amb-proc (cdr l)))))
        (k ((car l))))))))


(defmacro amb values
  (cons 'amb-proc (map (fun (v) `(fun () ,v)) values)))

(p
  (let ([i (amb 2 3 4)]
        [j (amb 5 6 7)])
    (if (= (* i j) 18)
      (cons i j)
      (amb))))
