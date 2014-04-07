(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
(assert-equal?
 (map fib '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
 (list 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987))


