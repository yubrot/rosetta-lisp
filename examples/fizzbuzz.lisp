(let loop ([i 1])
  (println
    (cond
      [(= 0 (% i 3) (% i 5)) "FizzBuzz"]
      [(= 0 (% i 3)) "Fizz"]
      [(= 0 (% i 5)) "Buzz"]
      [else (num->str i)]))
  (when (< i 40)
    (loop (+ i 1))))
