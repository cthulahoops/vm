(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
(map fib '(0 1 2 3 4 5 6 7 8 9 10))
