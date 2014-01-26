(define not  (lambda (x) (if x #f #t)))

(define even (lambda (x) (if (= x 0) #t (odd (- x 1)))))
(define odd (lambda (x) (if (= x 0) #f (even (- x 1)))))

(cons
  (cons (even 4) (even 5))
  (cons (odd 4) (odd 5)))
