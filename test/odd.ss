(define not  (lambda (x) (if (= x 0) 1 0)))

(define even (lambda (x) (if (= x 0) 1 (odd (- x 1)))))
(define odd (lambda (x) (if (= x 0) 0 (even (- x 1)))))

(cons
  (cons (even 4) (even 5))
  (cons (odd 4) (odd 5)))
