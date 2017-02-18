(def w 13)
(def h 13)
(def step 9)
(def board
  '((_ _ _ _ _ _ _ _ _ _ _ _ _)
    (_ _ _ _ _ _ _ _ _ _ _ _ _)
    (_ _ @ @ @ @ @ @ _ @ @ _ _)
    (_ _ @ @ @ @ @ @ _ @ @ _ _)
    (_ _ _ _ _ _ _ _ _ @ @ _ _)
    (_ _ @ @ _ _ _ _ _ @ @ _ _)
    (_ _ @ @ _ _ _ _ _ @ @ _ _)
    (_ _ @ @ _ _ _ _ _ @ @ _ _)
    (_ _ @ @ _ _ _ _ _ _ _ _ _)
    (_ _ @ @ _ @ @ @ @ @ @ _ _)
    (_ _ @ @ _ @ @ @ @ @ @ _ _)
    (_ _ _ _ _ _ _ _ _ _ _ _ _)
    (_ _ _ _ _ _ _ _ _ _ _ _ _)))

;;;;;;;;;;;

(defun for (l f)
  (map f l))

(defun iota (a b)
  (if (< a b)
    (cons a (iota (+ a 1) b))
    '()))

(defun gol-step (h w board)
  (defun at (y x)
    (if (or (not (<= 0 y (- h 1)))
            (not (<= 0 x (- w 1))))
      #f
      (= (nth x (nth y board)) '@)))

  (defun count-true l
    (let loop ([l l]
               [k 0])
      (if (nil? l)
        k
        (loop (cdr l)
              (if (car l) (+ k 1) k)))))

  (for (iota 0 h) (fun (y)
    (for (iota 0 w) (fun (x)
      (let ([y- (- y 1)]
            [y+ (+ y 1)]
            [x- (- x 1)]
            [x+ (+ x 1)]
            [current (at y x)]
            [border (count-true (at y- x-) (at y- x) (at y- x+) (at y x-) (at y x+) (at y+ x-) (at y+ x) (at y+ x+))])
        (cond
          [(and (not current) (= border 3)) '@]
          [(and current (<= 2 border 3)) '@]
          [else '_])))))))

(defun print-board (board)
  (map (fun (row) (apply p row)) board)
  (println))

(defun gol (h w step board)
  (let loop ([board board]
             [step step])
    (unless (= step 0)
      (print-board board)
      (loop (gol-step h w board) (- step 1)))))

(gol h w step board)
